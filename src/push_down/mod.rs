use std::collections::{HashMap as Map, HashSet as Set};
use std::hash::Hash;
use std::iter::once;

use Automaton;

#[derive(Clone)]
pub struct PushDown<S, A, G>
where
    S: Clone + Eq + Hash,
    A: Clone + Eq + Hash,
    G: Clone + Eq + Hash,
{
    pub initial: Set<S>,
    pub accepting: Set<S>,
    pub start: G,
    pub transitions: Map<(S, Option<A>, Option<G>), Set<(S, Vec<G>)>>,
}

impl<S, A, G> PushDown<S, A, G>
where
    S: Clone + Eq + Hash,
    A: Clone + Eq + Hash,
    G: Clone + Eq + Hash,
{
    pub fn traverse<I>(&self, input: I, mut states: Set<(S, Vec<G>)>) -> Set<(S, Vec<G>)>
    where
        I: Iterator<Item = A>,
    {
        states = self.lambda_closure(states);

        for label in input {
            let mut new_states = Set::new();

            for (from, stack) in states.into_iter() {
                if let Some(to) =
                    self.transitions
                        .get(&(from, Some(label.clone()), stack.last().cloned()))
                {
                    new_states.extend(to.iter().cloned());
                }
            }

            states = self.lambda_closure(new_states);
        }

        states
    }

    pub fn states(&self) -> Set<S> {
        let mut states = Set::new();

        states.extend(self.initial.iter().cloned());
        states.extend(self.accepting.iter().cloned());

        for ((from, _label, _top), to) in self.transitions.iter() {
            states.insert(from.clone());
            states.extend(to.iter().map(|(state, _stack)| state.clone()));
        }

        states
    }

    pub fn labels(&self) -> Set<A> {
        let mut labels = Set::new();

        for ((_from, label, _top), _to) in self.transitions.iter() {
            if let Some(label) = label {
                labels.insert(label.clone());
            }
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
        let start = vec![self.start.clone()];
        let states = self.traverse(
            input,
            self.initial
                .iter()
                .map(|state| (state.clone(), start.clone()))
                .collect(),
        );

        for (_state, stack) in states.iter() {
            if stack.is_empty() {
                return true;
            }
        }

        false
    }

    pub fn lambda_closure(&self, mut states: Set<(S, Vec<G>)>) -> Set<(S, Vec<G>)> {
        let mut not_checked: Vec<(S, Vec<G>)> = states.iter().cloned().collect();

        while let Some((from, stack)) = not_checked.pop() {
            if let Some(ref to) = self.transitions
                .get(&(from.clone(), None, stack.last().cloned()))
            {
                for state in to.iter() {
                    if states.get(state) == None {
                        states.insert(state.clone());
                        not_checked.push(state.clone());
                    }
                }
            }
        }

        states
    }
}

impl<S, A, G> Automaton for PushDown<S, A, G>
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
        let start = vec![self.start.clone()];
        let states = self.traverse(
            input,
            self.initial
                .iter()
                .map(|state| (state.clone(), start.clone()))
                .collect(),
        );

        for (state, _stack) in states.iter() {
            if self.accepting.get(state) != None {
                return true;
            }
        }

        false
    }
}
