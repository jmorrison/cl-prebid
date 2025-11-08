module.exports = {
  purge: [],
  darkMode: false, // or 'media' or 'class'
    content: [
	'../*reblocks*',
	'~/quicklisp/dists/ultralisp/software/40ants-*/**/*',
    ],
  theme: {
    extend: {},
  },
  variants: {
    extend: {},
  },
  plugins: [],
}
