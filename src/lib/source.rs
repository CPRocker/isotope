/// Isotope source code
#[derive(Debug, Clone)]
pub struct Source {
    /// Source content in memory as bytes
    content: Vec<u8>,

    /// Name of the source file
    name: Option<String>,
}

impl Source {
    /// Creates a `Source` by reading the entire content into memory.
    pub fn from_reader(mut reader: impl std::io::Read) -> std::io::Result<Self> {
        let mut content = Vec::new();
        reader.read_to_end(&mut content)?;

        Ok(Self {
            content,
            name: None,
        })
    }

    pub fn with_name(mut self, name: String) -> Self {
        self.name = Some(name);
        self
    }

    pub fn chars(&self) -> Result<std::str::Chars<'_>, std::str::Utf8Error> {
        Ok(std::str::from_utf8(&self.content)?.chars())
    }
}

impl miette::SourceCode for Source {
    /// Read the bytes for a specific span from this `Source`, keeping a
    /// certain number of lines before and after the span as context.
    ///
    /// Note: Assumes UTF-8 encoding of the content
    fn read_span<'a>(
        &'a self,
        span: &miette::SourceSpan,
        context_lines_before: usize,
        context_lines_after: usize,
    ) -> Result<Box<dyn miette::SpanContents<'a> + 'a>, miette::MietteError> {
        let start = span.offset();
        let end = start + span.len();

        if start >= self.content.len() || end > self.content.len() {
            return Err(miette::MietteError::OutOfBounds);
        }

        let substring = &self.content[start..end];
        let substring = std::str::from_utf8(substring).expect("invalid UTF-8 sequence"); // Note: Assumes UTF-8

        substring.read_span(span, context_lines_before, context_lines_after)
    }
}

impl From<Source> for miette::NamedSource<Source> {
    fn from(val: Source) -> Self {
        miette::NamedSource::new(val.name.clone().unwrap_or_default(), val).with_language("Isotope")
    }
}

#[derive(Debug)]
pub struct ParseSourceError;

impl std::str::FromStr for Source {
    type Err = ParseSourceError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self {
            content: s.as_bytes().to_vec(),
            name: None,
        })
    }
}
