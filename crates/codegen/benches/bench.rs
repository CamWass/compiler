#![feature(test)]

extern crate custom_alloc;
extern crate test;

use codegen::{self, Emitter};
use global_common::FileName;
use parser::Parser;
use std::hint::black_box;
use test::Bencher;

const COLORS_JS: &str = r#"
'use strict';
/**
 * Extract red color out of a color integer:
 *
 * 0x00DEAD -> 0x00
 *
 * @param  {Number} color
 * @return {Number}
 */
function red( color )
{
    let foo = 3.14;
    return color >> 16;
}
/**
 * Extract green out of a color integer:
 *
 * 0x00DEAD -> 0xDE
 *
 * @param  {Number} color
 * @return {Number}
 */
function green( color )
{
    return ( color >> 8 ) & 0xFF;
}
/**
 * Extract blue color out of a color integer:
 *
 * 0x00DEAD -> 0xAD
 *
 * @param  {Number} color
 * @return {Number}
 */
function blue( color )
{
    return color & 0xFF;
}
/**
 * Converts an integer containing a color such as 0x00DEAD to a hex
 * string, such as '#00DEAD';
 *
 * @param  {Number} int
 * @return {String}
 */
function intToHex( int )
{
    const mask = '#000000';
    const hex = int.toString( 16 );
    return mask.substring( 0, 7 - hex.length ) + hex;
}
/**
 * Converts a hex string containing a color such as '#00DEAD' to
 * an integer, such as 0x00DEAD;
 *
 * @param  {Number} num
 * @return {String}
 */
function hexToInt( hex )
{
    return parseInt( hex.substring( 1 ), 16 );
}
module.exports = {
    red,
    green,
    blue,
    intToHex,
    hexToInt,
};
"#;

const LARGE_PARTIAL_JS: &str = include_str!("large-partial.js");

fn bench_emitter(b: &mut Bencher, s: &str) {
    b.bytes = s.len() as _;

    let _ = ::testing::run_test(true, |cm, handler| {
        let fm = cm.new_source_file(FileName::Anon, s.into());
        let mut program_data = ast::ProgramData::default();

        let mut src_map_buf = vec![];
        let module = {
            let mut parser = Parser::new(Default::default(), &fm, &mut program_data);
            let m = parser
                .parse_module()
                .map_err(|e| e.into_diagnostic(handler).emit())
                .unwrap();

            for err in parser.take_errors() {
                err.into_diagnostic(handler).emit();
            }
            m
        };

        b.iter(|| {
            let mut buf = vec![];
            {
                let mut emitter = Emitter::new(
                    codegen::Config {
                        ..Default::default()
                    },
                    cm.clone(),
                    codegen::JsWriter::new("\n", &mut buf, Some(&mut src_map_buf)),
                    &program_data,
                );

                let _ = emitter.emit_module(&module);
            }
            black_box(buf);
            let srcmap = cm.build_source_map(&mut src_map_buf);
            black_box(srcmap);
        });
        Ok(())
    });
}

#[bench]
fn emit_colors(b: &mut Bencher) {
    bench_emitter(b, COLORS_JS)
}

#[bench]
fn emit_large(b: &mut Bencher) {
    bench_emitter(b, LARGE_PARTIAL_JS)
}
