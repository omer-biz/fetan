<!DOCTYPE html>
<html lang="{{ str_replace('_', '-', app()->getLocale()) }}">
    <head>
        <meta charset="utf-8">
        <meta name="viewport" content="width=device-width, initial-scale=1">
        <link href="https://fonts.googleapis.com/css2?family=JetBrains+Mono:wght@400;500;700&display=swap" rel="stylesheet">

        <title>Fetan</title>
        @vite(['resources/css/app.css', 'resources/js/app.js'])

        <script>
            let lessonInfo = localStorage.getItem('lessonInfo');
            let flags = lessonInfo ? JSON.parse(lessonInfo) : null;

            document.addEventListener("DOMContentLoaded", () => {
                const app = Elm.Main.init({
                    node: document.getElementById("elm-app"),
                    flags: flags,
                });

                app.ports.saveInfo.subscribe(function(state) {
                    localStorage.setItem('lessonInfo', JSON.stringify(state));
                });
            });
        </script>
    </head>
    <body class="bg-gray-700">
        <main id="elm-app"></main>
    </body>
    <foot>
    </foot>
</html>
