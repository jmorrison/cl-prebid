module.exports = {
    // purge: [],
    // darkMode: false, // or 'media' or 'class'
    content: [
	'../*reblocks*',
	'~/quicklisp/dists/ultralisp/software/40ants-*/**/*',
    ],
    theme: {
	extend: {
	    fontFamily: {
		custom: ['TanerArdali', 'sans-serif'],
	    },
	},
    },
    variants: {
	extend: {},
    },
    plugins: [],
}
