use std::collections::{HashMap as Map, HashSet as Set};
use std::hash::Hash;

use Automaton;

#[derive(Debug, Clone)]
pub enum State<S> {
    Going(S),
    Accepted,
    Rejected,
}

#[derive(Debug, Clone)]
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn substring_ab_or_suffix_ba() {
        use super::Direction::*;
        use super::State::*;

        let turing = TuringMachine {
            initial_state: 'o',
            transitions: vec![
                (('o', None), (Going('p'), None, Right)),
                (('p', Some('a')), (Going('q'), Some('a'), Right)),
                (('p', Some('b')), (Going('s'), Some('b'), Right)),
                (('q', Some('a')), (Going('q'), Some('a'), Right)),
                (('q', Some('b')), (Going('r'), Some('b'), Right)),
                (('r', Some('a')), (Going('r'), Some('a'), Right)),
                (('r', Some('b')), (Going('r'), Some('b'), Right)),
                (('s', Some('a')), (Going('t'), Some('a'), Right)),
                (('s', Some('b')), (Going('s'), Some('b'), Right)),
                (('t', Some('a')), (Going('q'), Some('a'), Right)),
                (('t', Some('b')), (Going('r'), Some('b'), Right)),
                (('t', None), (Accepted, None, Right)),
                (('r', None), (Accepted, None, Right)),
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
    }
}
