/** @type {import('tailwindcss').Config} */
const sliderWidths = [ 0, 200, 400, 600, 800, 1000 ];

export default {
  content: ["./examples/**/*.{elm,html,js,ts}", "./src/**/*.{elm,html,js,ts}"],
  safelist: sliderWidths.map((width) => `w-[${width}px]`),
  theme: {
    extend: {
      width: {
        0: "0px",
        200: "200px",
        400: "400px",
        600: "600px",
        800: "800px",
        1000: "1000px",
      },
    },
  },
  plugins: [],
};
