<x-layout>
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

    <main id="elm-app"></main>
</x-layout>
