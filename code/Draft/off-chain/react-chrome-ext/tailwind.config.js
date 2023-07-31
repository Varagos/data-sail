/** @type {import('tailwindcss').Config} */
module.exports = {
    content: ['./src/**/*.{js,jsx,ts,tsx}', './public/index.html'],
    // darkMode: false, // or 'media' or 'class'
    theme: {
        extend: {
            backgroundImage: {
                "gradient-radial": "radial-gradient(var(--tw-gradient-stops))",
                "gradient-conic":
                    "conic-gradient(from 180deg at 50% 50%, var(--tw-gradient-stops))",
            },
            fontFamily: {
                quicksand: ['"Quicksand"', "sans-serif"],
            },
        },
    },
    variants: {
        extend: {},
    },
    plugins: [],
}

