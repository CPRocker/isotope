use crate::generator::Generator;

pub struct Compiler<'de> {
    generator: Generator<'de>,
}

impl<'de> Compiler<'de> {
    pub fn new(file_contents: &'de str) -> Self {
        Self {
            generator: Generator::new(file_contents),
        }
    }

    pub fn compile(&mut self) -> String {
        self.generator.generate()
    }
}
