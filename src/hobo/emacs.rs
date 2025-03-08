//! Helpers for working with Emacs.

use std::collections::HashMap;

emacs::use_symbols!(symbol_name object_intervals);

pub struct EmacsListIntoIterator<'e> {
    cdr: emacs::Value<'e>,
}

impl<'e> Iterator for EmacsListIntoIterator<'e> {
    type Item = emacs::Value<'e>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.cdr.is_not_nil() {
            let car = self.cdr.car().unwrap();
            self.cdr = self.cdr.cdr().unwrap();
            return Some(car);
        }

        None
    }
}

/// Wrapper around an Emacs list value, which allows iterating over the list items.
pub struct EmacsList<'e>(emacs::Value<'e>);

impl<'e> IntoIterator for EmacsList<'e> {
    type Item = emacs::Value<'e>;
    type IntoIter = EmacsListIntoIterator<'e>;

    fn into_iter(self) -> Self::IntoIter {
        EmacsListIntoIterator { cdr: self.0 }
    }
}

impl<'e> emacs::FromLisp<'e> for EmacsList<'e> {
    fn from_lisp(value: emacs::Value<'e>) -> emacs::Result<Self> {
        Ok(EmacsList(value))
    }
}

#[derive(Debug)]
pub struct TextPropertyInterval<'e> {
    start: u32,
    end: u32,
    properties: HashMap<String, emacs::Value<'e>>,
}

/// Captures the contents and text properties of an Emacs string.
#[derive(Debug)]
pub struct StringWithProperties<'e> {
    string: String,
    intervals: Vec<TextPropertyInterval<'e>>,
}

impl<'e> StringWithProperties<'e> {
    pub fn string(&self) -> &str {
        &self.string
    }

    pub fn intervals(&self) -> &[TextPropertyInterval<'e>] {
        &self.intervals
    }
}

impl<'e> emacs::FromLisp<'e> for StringWithProperties<'e> {
    fn from_lisp(value: emacs::Value<'e>) -> emacs::Result<Self> {
        let string = String::from_lisp(value)?;

        let intervals = EmacsList::from_lisp(object_intervals.call(value.env, [value])?)?
            .into_iter()
            .map(|interval| {
                let mut iter = EmacsList::from_lisp(interval)?.into_iter();

                let start = iter
                    .next()
                    .ok_or_else(|| anyhow::anyhow!("Invalid interval: expected start"))?
                    .into_rust::<u32>()?;

                let end = iter
                    .next()
                    .ok_or_else(|| anyhow::anyhow!("Invalid interval: expected end"))?
                    .into_rust::<u32>()?;

                let properties_value = iter
                    .next()
                    .ok_or_else(|| anyhow::anyhow!("Invalid interval: expected properties"))?;

                let properties = EmacsList::from_lisp(properties_value)?
                    .into_iter()
                    .collect::<Vec<emacs::Value>>()
                    .chunks_exact(2)
                    .map(|c| {
                        Ok((
                            symbol_name.call(value.env, [c[0]])?.into_rust::<String>()?,
                            c[1],
                        ))
                    })
                    .collect::<Result<HashMap<String, emacs::Value>, emacs::Error>>()?;

                Ok(TextPropertyInterval {
                    start,
                    end,
                    properties,
                })
            })
            .collect::<Result<Vec<TextPropertyInterval<'e>>, emacs::Error>>()?;

        Ok(StringWithProperties { string, intervals })
    }
}
