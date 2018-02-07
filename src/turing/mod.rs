use std::collections::{HashMap as Map, HashSet as Set};
use std::hash::Hash;

use Automaton;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum State<S> {
    Going(S),
    Accepted,
    Rejected,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Direction {
    Left,
    Stay,
    Right,
}

#[derive(Debug, Clone)]
pub struct TuringMachine<S, T>
where
    S: Clone + Eq + Hash + ::std::fmt::Debug,
    T: Clone + Eq + Hash + ::std::fmt::Debug,
{
    pub initial_state: S,
    pub transitions: Map<(S, Option<T>), (State<S>, Option<T>, Direction)>,
}

impl<S, T> TuringMachine<S, T>
where
    S: Clone + Eq + Hash + ::std::fmt::Debug,
    T: Clone + Eq + Hash + ::std::fmt::Debug,
{
    pub fn states(&self) -> Set<S> {
        let mut states = Set::new();

        states.insert(self.initial_state.clone());

        for (to_state, _to_label, _direction) in self.transitions.values() {
            if let State::Going(to_state) = to_state {
                states.insert(to_state.clone());
            }
        }

        states
    }

    pub fn used_states(&self) -> Set<S> {
        let symbols = self.symbols();

        let mut used_states = Set::new();
        used_states.insert(self.initial_state.clone());

        let mut not_checked = vec![&self.initial_state];
        while let Some(from_state) = not_checked.pop() {
            for from_symbol in symbols.iter().cloned() {
                if let Some((to_state, _to_symbol, _direction)) = self.transitions
                    .get(&(from_state.clone(), Some(from_symbol)))
                {
                    if let State::Going(to_state) = to_state {
                        if used_states.get(to_state) == None {
                            used_states.insert(to_state.clone());
                            not_checked.push(to_state);
                        }
                    }
                }
            }

            if let Some((to_state, _to_symbol, _direction)) =
                self.transitions.get(&(from_state.clone(), None))
            {
                if let State::Going(to_state) = to_state {
                    if used_states.get(to_state) == None {
                        used_states.insert(to_state.clone());
                        not_checked.push(to_state);
                    }
                }
            }
        }

        used_states
    }

    pub fn symbols(&self) -> Set<T> {
        let mut symbols = Set::new();

        for ((_from_state, from_symbol), (_to_state, to_symbol, _direction)) in
            self.transitions.iter()
        {
            if let Some(from_symbol) = from_symbol {
                symbols.insert(from_symbol.clone());
            }

            if let Some(to_symbol) = to_symbol {
                symbols.insert(to_symbol.clone());
            }
        }

        symbols
    }

    pub fn used_symbols(&self) -> Set<T> {
        let used_states = self.used_states();

        let mut used_symbols = Set::new();

        for ((from_state, from_symbol), (_to_state, to_symbol, _direction)) in
            self.transitions.iter()
        {
            if used_states.get(from_state) != None {
                if let Some(from_symbol) = from_symbol {
                    used_symbols.insert(from_symbol.clone());
                }

                if let Some(to_symbol) = to_symbol {
                    used_symbols.insert(to_symbol.clone());
                }
            }
        }

        used_symbols
    }
}

impl<S, T> Automaton for TuringMachine<S, T>
where
    S: Clone + Eq + Hash + ::std::fmt::Debug,
    T: Clone + Eq + Hash + ::std::fmt::Debug,
{
    type Alphabet = Option<T>;

    fn accepts<I>(&self, input: I) -> bool
    where
        I: IntoIterator<Item = Option<T>>,
    {
        let input = input.into_iter();

        let mut tape = Vec::with_capacity(input.size_hint().0 + 1);
        tape.push(None);
        tape.extend(input);

        let mut state = self.initial_state.clone();
        let mut i = 0;

        loop {
            if let Some((new_state, new_symbol, direction)) =
                self.transitions.get(&(state, tape[i].clone()))
            {
                match new_state {
                    State::Going(new_state) => state = new_state.clone(),
                    State::Accepted => return true,
                    State::Rejected => return false,
                }

                tape[i] = new_symbol.clone();

                match direction {
                    Direction::Left => if i > 0 {
                        i -= 1
                    },
                    Direction::Stay => {},
                    Direction::Right => {
                        i += 1;
                        if i == tape.len() {
                            tape.push(None)
                        }
                    },
                }
            } else {
                return false;
            }
        }
    }
}

impl<S, T> From<::Deter<S, T>> for TuringMachine<Option<S>, T>
where
    S: Clone + Eq + Hash + ::std::fmt::Debug,
    T: Clone + Eq + Hash + ::std::fmt::Debug,
{
    fn from(deter: ::Deter<S, T>) -> TuringMachine<Option<S>, T> {
        let ::Deter {
            initial_state,
            accepting_states,
            transitions,
        } = deter;

        let mut transitions: Map<_, _> = transitions
            .into_iter()
            .map(|((from, label), to)| {
                (
                    (Some(from), Some(label)),
                    (State::Going(Some(to)), None, Direction::Right),
                )
            })
            .collect();

        transitions.insert(
            (None, None),
            (State::Going(Some(initial_state)), None, Direction::Right),
        );

        transitions.extend(accepting_states.into_iter().map(|state| {
            (
                (Some(state), None),
                (State::Accepted, None, Direction::Stay),
            )
        }));

        TuringMachine {
            initial_state: None,
            transitions,
        }
    }
}

impl<S, T> From<::NonDeter<S, T>> for TuringMachine<Option<usize>, T>
where
    S: Clone + Eq + Hash + ::std::fmt::Debug,
    T: Clone + Eq + Hash + ::std::fmt::Debug,
{
    fn from(non_deter: ::NonDeter<S, T>) -> TuringMachine<Option<usize>, T> {
        TuringMachine::from(::Deter::from(non_deter))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn substring_ab_or_suffix_ba() {
        use super::Direction::*;
        use super::State::*;

        let deter: TuringMachine<_, _> = ::Deter {
            initial_state: 'p',
            accepting_states: vec!['r', 't'].into_iter().collect(),
            transitions: vec![
                (('p', 'a'), 'q'),
                (('p', 'b'), 's'),
                (('q', 'a'), 'q'),
                (('q', 'b'), 'r'),
                (('r', 'a'), 'r'),
                (('r', 'b'), 'r'),
                (('s', 'a'), 't'),
                (('s', 'b'), 's'),
                (('t', 'a'), 'q'),
                (('t', 'b'), 'r'),
            ].into_iter()
                .collect(),
        }.into();

        let turing = TuringMachine {
            initial_state: None,
            transitions: vec![
                ((None, None), (Going(Some('p')), None, Right)),
                ((Some('p'), Some('a')), (Going(Some('q')), None, Right)),
                ((Some('p'), Some('b')), (Going(Some('s')), None, Right)),
                ((Some('q'), Some('a')), (Going(Some('q')), None, Right)),
                ((Some('q'), Some('b')), (Going(Some('r')), None, Right)),
                ((Some('r'), Some('a')), (Going(Some('r')), None, Right)),
                ((Some('r'), Some('b')), (Going(Some('r')), None, Right)),
                ((Some('s'), Some('a')), (Going(Some('t')), None, Right)),
                ((Some('s'), Some('b')), (Going(Some('s')), None, Right)),
                ((Some('t'), Some('a')), (Going(Some('q')), None, Right)),
                ((Some('t'), Some('b')), (Going(Some('r')), None, Right)),
                ((Some('t'), None), (Accepted, None, Stay)),
                ((Some('r'), None), (Accepted, None, Stay)),
            ].into_iter()
                .collect(),
        };

        assert!(turing.accepts("aba".chars().map(|c| Some(c))));
        assert!(turing.accepts("abaa".chars().map(|c| Some(c))));
        assert!(turing.accepts("ba".chars().map(|c| Some(c))));
        assert!(turing.accepts("aaabba".chars().map(|c| Some(c))));
        assert!(!turing.accepts("".chars().map(|c| Some(c))));
        assert!(!turing.accepts("bbb".chars().map(|c| Some(c))));
        assert!(!turing.accepts("baa".chars().map(|c| Some(c))));
        assert!(!turing.accepts("bbaa".chars().map(|c| Some(c))));

        assert!(deter.initial_state == turing.initial_state);
        assert!(deter.transitions == turing.transitions);
    }
}
