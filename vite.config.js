import { defineConfig } from "vite";
import laravel from "laravel-vite-plugin";
import elmPlugin from "vite-plugin-elm";

export default defineConfig({
    plugins: [
        laravel({
            input: ["resources/css/app.css", "resources/js/app.js"],
            refresh: true,
        }),
        elmPlugin(),
    ],
});
