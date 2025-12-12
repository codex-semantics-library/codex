import resolve from '@rollup/plugin-node-resolve';
import commonjs from '@rollup/plugin-commonjs';
import wasm from '@rollup/plugin-wasm';
import terser from '@rollup/plugin-terser';

export default {
    input: 'bundle-input.js',
    output: [
	{
	    file: 'bundle-output.js',
	    format: 'iife',
	    name: 'BundleOutput',
	    sourcemap: 'inline'
	},
	{	   
	    file: 'bundle-output.min.js',
	    format: 'iife',
	    name: 'BundleOutput',
	    plugins: [terser()]
	}
    ],
    plugins: [
	resolve(),
	commonjs(),
	wasm({ maxFileSize: 100000000 })
    ]
};
