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
    S: Clone + Eq + Hash,
    T: Clone + Eq + Hash,
{
    pub initial_state: S,
    pub transitions: Map<(S, Option<T>), (State<S>, Option<T>, Direction)>,
}

impl<S, T> Automaton for TuringMachine<S, T>
where
    S: Clone + Eq + Hash,
    T: Clone + Eq + Hash,
{
    type Alphabet = Option<T>;

    fn accepts<I>(&self, input: I) -> bool
    where
        I: Iterator<Item = Option<T>>,
    {
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
