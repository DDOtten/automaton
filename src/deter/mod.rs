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
    pub initial: S,
    /// The set of accepting states.
    pub accepting: Set<S>,
    /// The transitions: from a state we move with a label to the new state.
    pub transitions: Map<(S, A), S>,
}

impl<S, A> Deter<S, A>
where
    S: Clone + Eq + Hash,
    A: Clone + Eq + Hash,
{
    pub fn similar(&self) -> Set<(S, S)> {
        let labels = self.labels();
        let reachable_states = self.reachable_states();

        let mut similar = Map::new();

        let mut iter1 = reachable_states.iter();
        while let Some(state1) = iter1.next() {
            let mut iter2 = iter1.clone();
            while let Some(state2) = iter2.next() {
                similar.insert(
                    (state1.clone(), state2.clone()),
                    (self.accepting.get(&state1) == None) == (self.accepting.get(state2) == None),
                );
            }
        }

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
                            let to1 = self.transitions
                                .get(&(from1.clone(), label.clone()))
                                .expect("no transition");
                            let to2 = self.transitions
                                .get(&(from2.clone(), label.clone()))
                                .expect("no transition");

                            // If we can reach to states that ere not marked similar.
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

        let mut pairs = Set::new();

        similar.into_iter().for_each(|((state1, state2), simular)| {
            if simular {
                pairs.insert((state1, state2));
            }
        });

        pairs
    }

    pub fn minimal(&self) -> Deter<S, A> {
        let reachable_states = self.reachable_states();
        let similar = self.similar();

        let mut minimal_states = Set::new();
        minimal_states.insert(self.initial.clone());

        'outer: for state in reachable_states.iter() {
            for (state1, state2) in similar.iter() {
                if (state == state1 && minimal_states.get(state2) == None)
                    || (state == state2 && minimal_states.get(state1) == None)
                {
                    continue 'outer;
                }
            }

            minimal_states.insert(state.clone());
        }

        let mut minimal = Deter {
            initial: self.initial.clone(),
            accepting: &self.accepting & &minimal_states,
            transitions: Map::new(),
        };

        'outer: for ((from, label), to) in self.transitions.iter() {
            for (state1, state2) in similar.iter() {
                if from == state1 && minimal_states.get(state2) == None {
                    minimal
                        .transitions
                        .insert((state2.clone(), label.clone()), to.clone());
                    continue 'outer;
                } else if from == state2 && minimal_states.get(state1) == None {
                    minimal
                        .transitions
                        .insert((state1.clone(), label.clone()), to.clone());
                    continue 'outer;
                }
            }
        }

        minimal
    }

    pub fn complement(&self) -> Deter<S, A> {
        Deter {
            initial: self.initial.clone(),
            accepting: &self.states() - &self.accepting,
            transitions: self.transitions.clone(),
        }
    }

    fn combine<T>(&self, other: &Deter<T, A>) -> Deter<(S, T), A>
    where
        T: Clone + Eq + Hash,
    {
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

        let initial = (self.initial.clone(), other.initial.clone());

        Deter {
            initial,
            accepting: Set::new(),
            transitions,
        }
    }

    pub fn union<'a, T>(&'a self, other: &'a Deter<T, A>) -> Deter<(S, T), A>
    where
        T: Clone + Eq + Hash,
    {
        let mut automaton = Deter::combine(&self, &other);

        for self_state in self.accepting.iter() {
            for other_state in other.states() {
                automaton
                    .accepting
                    .insert((self_state.clone(), other_state.clone()));
            }
        }
        for other_state in other.accepting.iter() {
            for self_state in self.states() {
                automaton
                    .accepting
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

        for self_state in self.accepting.iter() {
            for other_state in other.accepting.iter() {
                automaton
                    .accepting
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

        for self_state in self.accepting.iter() {
            for other_state in other.states() {
                if other.accepting.get(&other_state) == None {
                    automaton
                        .accepting
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
        for self_state in self.accepting.iter() {
            for other_state in other.states() {
                union.insert((self_state.clone(), other_state.clone()));
            }
        }
        for other_state in other.accepting.iter() {
            for self_state in self.states() {
                union.insert((self_state.clone(), other_state.clone()));
            }
        }

        let mut intersection = Set::new();
        for self_state in self.accepting.iter() {
            for other_state in other.accepting.iter() {
                intersection.insert((self_state.clone(), other_state.clone()));
            }
        }

        automaton.accepting = &union - &intersection;

        automaton
    }
}

impl<S, A> Automaton for Deter<S, A>
where
    S: Clone + Eq + Hash,
    A: Clone + Eq + Hash,
{
    type State = S;
    type Alphabet = A;

    type Situation = S;

    fn accepts<I>(&self, input: I) -> bool
    where
        I: Iterator<Item = A>,
    {
        self.accepting.get(&self.traverse(input, self.initial.clone())) != None
    }

    fn traverse<I>(&self, input: I, mut state: S) -> S
    where
        I: Iterator<Item = A>,
    {
        for label in input {
            state = self.transitions
                .get(&(state, label))
                .expect("no transition")
                .clone();
        }

        state
    }

    fn states(&self) -> Set<S> {
        let mut states = Set::new();

        states.insert(self.initial.clone());
        states.extend(self.accepting.iter().cloned());

        for ((from, _label), to) in self.transitions.iter() {
            states.insert(from.clone());
            states.insert(to.clone());
        }

        states
    }

    fn reachable_states(&self) -> Set<S> {
        let labels = self.labels();

        let mut reachable_states = Set::new();
        reachable_states.insert(self.initial.clone());

        let mut not_checked = vec![&self.initial];
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

    fn labels(&self) -> Set<A> {
        let mut labels = Set::new();

        for ((_from, label), _to) in self.transitions.iter() {
            labels.insert(label.clone());
        }

        labels
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
