use std::collections::{HashMap as Map, HashSet as Set};
use std::hash::Hash;

use Automaton;

#[derive(Debug, Clone)]
pub enum State<S> {
    State(S),
    Accepted,
    Rejected,
}

#[derive(Debug, Clone)]
pub enum Direction {
    Left,
    Right,
    Stay,
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
