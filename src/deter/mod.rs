use std::collections::{HashMap as Map, HashSet as Set};
use std::hash::Hash;
use std::ops;

use Automaton;

extern crate disjoint_sets;

#[derive(Debug, Clone)]
pub struct Deter<S, A>
where
    S: Clone + Eq + Hash,
    A: Clone + Eq + Hash,
{
    /// The initial state of the automaton.
    pub initial_state: S,
    /// The set of accepting states.
    pub accepting_states: Set<S>,
    /// The transitions: from a state we move with a label to the new state.
    pub transitions: Map<(S, A), S>,
}

impl<S, A> Deter<S, A>
where
    S: Clone + Eq + Hash,
    A: Clone + Eq + Hash,
{
    pub fn traverse<I>(&self, input: I, mut state: S) -> Option<S>
    where
        I: Iterator<Item = A>,
    {
        for label in input {
            state = self.transitions.get(&(state, label))?.clone();
        }

        Some(state)
    }

    pub fn states(&self) -> Set<S> {
        let mut states = Set::new();

        states.insert(self.initial_state.clone());
        states.extend(self.accepting_states.iter().cloned());

        for ((from, _label), to) in self.transitions.iter() {
            states.insert(from.clone());
            states.insert(to.clone());
        }

        states
    }

    pub fn reachable_states(&self) -> Set<S> {
        let labels = self.labels();

        let mut reachable_states = Set::new();
        reachable_states.insert(self.initial_state.clone());

        let mut not_checked = vec![&self.initial_state];
        while let Some(from) = not_checked.pop() {
            for label in labels.iter() {
                if let Some(to) = self.transitions.get(&(from.clone(), label.clone())) {
                    if reachable_states.get(to) == None {
                        reachable_states.insert(to.clone());
                        not_checked.push(to);
                    }
                }
            }
        }

        reachable_states
    }

    pub fn labels(&self) -> Set<A> {
        let mut labels = Set::new();

        for ((_from, label), _to) in self.transitions.iter() {
            labels.insert(label.clone());
        }

        labels
    }

    pub fn partition(&self) -> Map<S, usize> {
        Map::new()
    }

    pub fn minimal(&self) -> Deter<usize, A> {
        let labels = self.labels();
        let reachable_states = self.reachable_states();

        let mut similar = Map::new();

        // We create all the unique pairs of states and mark them similar if and only if
        // they are both accepting_states or both not accepting_states.
        let mut iter1 = reachable_states.iter();
        while let Some(state1) = iter1.next() {
            let mut iter2 = iter1.clone();
            while let Some(state2) = iter2.next() {
                similar.insert(
                    (state1.clone(), state2.clone()),
                    (self.accepting_states.get(&state1) == None)
                        == (self.accepting_states.get(state2) == None),
                );
            }
        }

        // We mark states not similar if there is a label where the resuling states are not similar.
        // We keep repeating this while we have made a change in the last iteration.
        let mut changed = true;
        while changed {
            changed = false;

            let mut iter1 = reachable_states.iter();
            while let Some(from1) = iter1.next() {
                let mut iter2 = iter1.clone();
                while let Some(from2) = iter2.next() {
                    // If the states are marked similar.
                    if similar.get(&(from1.clone(), from2.clone())) == Some(&true) {
                        for label in labels.iter() {
                            if let (Some(to1), Some(to2)) = (
                                self.transitions.get(&(from1.clone(), label.clone())),
                                self.transitions.get(&(from2.clone(), label.clone())),
                            ) {
                                // If we can reach states that are not marked similar.
                                if similar.get(&(to1.clone(), to2.clone())) == Some(&false)
                                    || similar.get(&(to2.clone(), to1.clone())) == Some(&false)
                                {
                                    similar.insert((from1.clone(), from2.clone()), false);
                                    changed = true;
                                    break;
                                }
                            }
                        }
                    }
                }
            }
        }

        let mut partition: Map<S, usize> = Map::new();

        let mut max = 0;

        'outer: for higher_state in reachable_states.iter() {
            partition.insert(higher_state.clone(), {
                let mut ret = max;

                for lower_state in reachable_states.iter() {
                    if lower_state == higher_state {
                        max += 1;
                        break;
                    } else if similar.get(&(lower_state.clone(), higher_state.clone()))
                        == Some(&true)
                    {
                        let value = partition.get(lower_state).unwrap().clone();
                        ret = value;
                        break;
                    }
                }

                ret
            });
        }

        Deter {
            initial_state: *partition.get(&self.initial_state).unwrap(),
            accepting_states: self.accepting_states
                .iter()
                .filter_map(|state| partition.get(state).cloned())
                .collect(),
            transitions: self.transitions
                .iter()
                .filter_map(|((from, label), to)| {
                    Some(((*partition.get(from)?, label.clone()), *partition.get(to)?))
                })
                .collect(),
        }
    }

    pub fn complement(&self) -> Deter<S, A> {
        Deter {
            initial_state: self.initial_state.clone(),
            accepting_states: &self.states() - &self.accepting_states,
            transitions: self.transitions.clone(),
        }
    }

    fn combine<T>(&self, other: &Deter<T, A>) -> Deter<(S, T), A>
    where
        T: Clone + Eq + Hash,
    {
        let initial_state = (self.initial_state.clone(), other.initial_state.clone());

        let mut transitions = Map::new();
        for ((self_from, self_label), self_to) in self.transitions.iter() {
            for ((other_from, other_label), other_to) in other.transitions.iter() {
                if self_label == other_label {
                    transitions.insert(
                        ((self_from.clone(), other_from.clone()), self_label.clone()),
                        (self_to.clone(), other_to.clone()),
                    );
                }
            }
        }

        Deter {
            initial_state,
            accepting_states: Set::new(),
            transitions,
        }
    }

    pub fn union<'a, T>(&'a self, other: &'a Deter<T, A>) -> Deter<(S, T), A>
    where
        T: Clone + Eq + Hash,
    {
        let mut automaton = Deter::combine(&self, &other);

        for self_state in self.accepting_states.iter() {
            for other_state in other.states() {
                automaton
                    .accepting_states
                    .insert((self_state.clone(), other_state.clone()));
            }
        }
        for other_state in other.accepting_states.iter() {
            for self_state in self.states() {
                automaton
                    .accepting_states
                    .insert((self_state.clone(), other_state.clone()));
            }
        }

        automaton
    }

    pub fn intersection<'a, T>(&'a self, other: &'a Deter<T, A>) -> Deter<(S, T), A>
    where
        T: Clone + Eq + Hash,
    {
        let mut automaton = Deter::combine(&self, &other);

        for self_state in self.accepting_states.iter() {
            for other_state in other.accepting_states.iter() {
                automaton
                    .accepting_states
                    .insert((self_state.clone(), other_state.clone()));
            }
        }

        automaton
    }

    pub fn difference<'a, T>(&'a self, other: &'a Deter<T, A>) -> Deter<(S, T), A>
    where
        T: Clone + Eq + Hash,
    {
        let mut automaton = Deter::combine(&self, &other);

        for self_state in self.accepting_states.iter() {
            for other_state in other.states() {
                if other.accepting_states.get(&other_state) == None {
                    automaton
                        .accepting_states
                        .insert((self_state.clone(), other_state.clone()));
                }
            }
        }

        automaton
    }

    pub fn symmetric_difference<'a, T>(&'a self, other: &'a Deter<T, A>) -> Deter<(S, T), A>
    where
        T: Clone + Eq + Hash,
    {
        let mut automaton = Deter::combine(&self, &other);

        let mut union = Set::new();
        for self_state in self.accepting_states.iter() {
            for other_state in other.states() {
                union.insert((self_state.clone(), other_state.clone()));
            }
        }
        for other_state in other.accepting_states.iter() {
            for self_state in self.states() {
                union.insert((self_state.clone(), other_state.clone()));
            }
        }

        let mut intersection = Set::new();
        for self_state in self.accepting_states.iter() {
            for other_state in other.accepting_states.iter() {
                intersection.insert((self_state.clone(), other_state.clone()));
            }
        }

        automaton.accepting_states = &union - &intersection;

        automaton
    }
}

impl<S, A> Automaton for Deter<S, A>
where
    S: Clone + Eq + Hash,
    A: Clone + Eq + Hash,
{
    type Alphabet = A;

    fn accepts<I>(&self, input: I) -> bool
    where
        I: Iterator<Item = A>,
    {
        if let Some(state) = self.traverse(input, self.initial_state.clone()) {
            self.accepting_states.get(&state) != None
        } else {
            false
        }
    }
}

impl<'a, S, A> ops::Not for &'a Deter<S, A>
where
    S: Clone + Eq + Hash,
    A: Clone + Eq + Hash,
{
    type Output = Deter<S, A>;

    fn not(self) -> Deter<S, A> {
        self.complement()
    }
}

impl<'a, 'b, S, T, A> ops::BitOr<&'b Deter<T, A>> for &'a Deter<S, A>
where
    S: Clone + Eq + Hash,
    T: Clone + Eq + Hash,
    A: Clone + Eq + Hash,
{
    type Output = Deter<(S, T), A>;

    fn bitor(self, rhs: &Deter<T, A>) -> Deter<(S, T), A> {
        self.union(rhs)
    }
}

impl<'a, 'b, S, T, A> ops::BitAnd<&'b Deter<T, A>> for &'a Deter<S, A>
where
    S: Clone + Eq + Hash,
    T: Clone + Eq + Hash,
    A: Clone + Eq + Hash,
{
    type Output = Deter<(S, T), A>;

    fn bitand(self, rhs: &Deter<T, A>) -> Deter<(S, T), A> {
        self.intersection(rhs)
    }
}

impl<'a, 'b, S, T, A> ops::BitXor<&'b Deter<T, A>> for &'a Deter<S, A>
where
    S: Clone + Eq + Hash,
    T: Clone + Eq + Hash,
    A: Clone + Eq + Hash,
{
    type Output = Deter<(S, T), A>;

    fn bitxor(self, rhs: &Deter<T, A>) -> Deter<(S, T), A> {
        self.symmetric_difference(rhs)
    }
}

impl<'a, 'b, S, T, A> ops::Sub<&'b Deter<T, A>> for &'a Deter<S, A>
where
    S: Clone + Eq + Hash,
    T: Clone + Eq + Hash,
    A: Clone + Eq + Hash,
{
    type Output = Deter<(S, T), A>;

    fn sub(self, rhs: &Deter<T, A>) -> Deter<(S, T), A> {
        self.difference(rhs)
    }
}
