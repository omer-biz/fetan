@php
$user = Auth::user();
@endphp

<x-layout>
    <script>
     @auth
        let flags = {
            lessonIdx: {{ $user->lessonIdx }},
            metrics: {
                speed: {
                    new: {{ $user->speed_new }},
                    old: {{ $user->speed_old }},
                },
                accuracy: {
                    new: {{ $user->accuracy_new }},
                    old: {{ $user->accuracy_old }},
                },
                score: {
                    new: {{ $user->score_new }},
                    old: {{ $user->score_old }},
                },
            }
        };
     @endauth

     @guest
        let lessonInfo = localStorage.getItem('lessonInfo');
        let flags = lessonInfo ? JSON.parse(lessonInfo) : null;
     @endguest

        function uploadInfo(info) {
            if (flags != null) {
                const formData = new FormData();
                formData.append("_token", "{{ csrf_token() }}");

                formData.append("lessonIdx", info.lessonIdx);

                formData.append("speed.new", info.metrics.speed.new);
                formData.append("speed.old", info.metrics.speed.old);

                formData.append("accuracy.new", info.metrics.accuracy.new);
                formData.append("accuracy.old", info.metrics.accuracy.old);

                formData.append("score.new", info.metrics.score.new);
                formData.append("score.old", info.metrics.score.old);

                const _ = fetch("{{ route('info.update') }}", {
                    method: 'POST',
                    body: formData
                });
            }
        }

        document.addEventListener("DOMContentLoaded", () => {
            const app = Elm.Main.init({
                node: document.getElementById("elm-app"),
                flags: flags,
            });

            app.ports.saveInfo.subscribe(function(state) {
                localStorage.setItem('lessonInfo', JSON.stringify(state));

                @auth
                uploadInfo(state)
                @endauth
            });
        });
    </script>

    <main id="elm-app"></main>
</x-layout>
