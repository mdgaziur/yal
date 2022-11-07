use parking_lot::RwLock;

#[derive(Debug, Copy, Clone, Default, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct InternedString(usize);

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Eq, Ord)]
pub struct InternedValue(usize);

pub static INTERNER: RwLock<Interner> = RwLock::new(Interner::new());

#[derive(Debug)]
pub struct Interner {
    strings: Vec<String>,
}

impl Interner {
    pub const fn new() -> Self {
        Self {
            strings: Vec::new(),
        }
    }

    pub fn intern_string(&mut self, string: String) -> InternedString {
        for (idx, interned_string) in self.strings.iter().enumerate() {
            if interned_string == &string {
                return InternedString(idx);
            }
        }

        let id = self.strings.len();
        self.strings.push(string);

        InternedString(id)
    }

    pub fn get_intern_addr_for_string(&self, string: &str) -> Option<InternedString> {
        for (idx, interned_string) in self.strings.iter().enumerate() {
            if interned_string == string {
                return Some(InternedString(idx));
            }
        }

        None
    }

    pub fn get_interned_string(&self, id: InternedString) -> &str {
        &self.strings[id.0]
    }
}
