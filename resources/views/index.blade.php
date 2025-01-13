<!DOCTYPE html>
<html lang="{{ str_replace('_', '-', app()->getLocale()) }}">
    <head>
        <meta charset="utf-8">
        <meta name="viewport" content="width=device-width, initial-scale=1">

        <title>Fetan</title>
        @vite(['resources/css/app.css', 'resources/js/app.js'])

        <script>
            document.addEventListener("DOMContentLoaded", () => {
                const app = Elm.Main.init({
                    node: document.getElementById("elm-app"),
                    flags: null,
                });
            });
        </script>
    </head>
    <body class="bg-gray-500">
        <main id="elm-app"></main>
    </body>
</html>
