use std::collections::{HashMap as Map, HashSet as Set};
use std::hash::Hash;

use Automaton;

#[derive(Clone)]
pub struct DeterPushDown<S, A, G>
where
    S: Clone + Eq + Hash,
    A: Clone + Eq + Hash,
    G: Clone + Eq + Hash,
{
    pub initial_state: S,
    pub initial_stack: Vec<G>,
    pub accepting_states: Set<S>,
    pub transitions: Map<(S, A, Option<G>), (S, Vec<G>)>,
}

impl<S, A, G> DeterPushDown<S, A, G>
where
    S: Clone + Eq + Hash,
    A: Clone + Eq + Hash,
    G: Clone + Eq + Hash,
{
    pub fn traverse<I>(&self, input: I, (mut state, mut stack): (S, Vec<G>)) -> Option<(S, Vec<G>)>
    where
        I: Iterator<Item = A>,
    {
        for label in input {
            let (new_state, new_stack) = self.transitions
                .get(&(state, label.clone(), stack.pop()))?
                .clone();

            state = new_state;
            stack.extend_from_slice(&new_stack);
        }

        Some((state, stack))
    }

    pub fn states(&self) -> Set<S> {
        let mut states = Set::new();

        states.insert(self.initial_state.clone());
        states.extend(self.accepting_states.iter().cloned());

        for ((from, _label, _top), (to, _stack)) in self.transitions.iter() {
            states.insert(from.clone());
            states.insert(to.clone());
        }

        states
    }

    pub fn labels(&self) -> Set<A> {
        let mut labels = Set::new();

        for ((_from, label, _top), _to) in self.transitions.iter() {
            labels.insert(label.clone());
        }

        labels
    }

    pub fn symbols(&self) -> Set<G> {
        let mut symbols = Set::new();

        for ((_from, _label, top), _to) in self.transitions.iter() {
            if let Some(top) = top {
                symbols.insert(top.clone());
            }
        }

        symbols
    }

    pub fn accepts_empty_stack<I>(&self, input: I) -> bool
    where
        I: Iterator<Item = A>,
    {
        let states = self.traverse(
            input,
            (self.initial_state.clone(), self.initial_stack.clone()),
        );

        for (_state, stack) in states.iter() {
            if stack.is_empty() {
                return true;
            }
        }

        false
    }
}

impl<S, A, G> Automaton for DeterPushDown<S, A, G>
where
    S: Clone + Eq + Hash,
    A: Clone + Eq + Hash,
    G: Clone + Eq + Hash,
{
    type Alphabet = A;

    fn accepts<I>(&self, input: I) -> bool
    where
        I: Iterator<Item = A>,
    {
        let states = self.traverse(
            input,
            (self.initial_state.clone(), self.initial_stack.clone()),
        );

        for (state, _stack) in states.iter() {
            if self.accepting_states.get(state) != None {
                return true;
            }
        }

        false
    }
}
