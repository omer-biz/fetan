@php
$user = Auth::user();
@endphp

<x-layout>
    <script>
     @auth
        localStorage.setItem("lessonInfo.bak", localStorage.getItem("lessonInfo"));
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


            const modal = document.getElementById("onboarding-modal");
            const modalContent = document.getElementById("modal-content");

            const showModal = () => {
                modal.classList.remove("hidden");
                setTimeout(() => {
                    modalContent.classList.remove("scale-95", "opacity-0");
                    modalContent.classList.add("scale-100", "opacity-100");
                }, 10);
            };

            const hideModal = () => {
                modalContent.classList.add("scale-95", "opacity-0");
                modalContent.classList.remove("scale-100", "opacity-100");
                setTimeout(() => {
                    modal.classList.add("hidden");
                }, 300);
                localStorage.setItem("init", true);
            };

            if (!localStorage.getItem("init")) {
                showModal();
            }

            modal.addEventListener("click", (e) => {
                if (e.target === modal) {
                    hideModal();
                }
            });

            window.explore_as_guest.addEventListener("click", hideModal);
            window.init_signup.addEventListener("click", hideModal);
        });
    </script>

    <main id="elm-app"></main>

<div id="onboarding-modal" class="fixed inset-0 bg-gray-900 bg-opacity-70 flex items-center justify-center z-50 hidden">
    <div
        id="modal-content"
        class="bg-gray-700 text-white rounded-lg shadow-lg p-6 max-w-lg w-full transform scale-95 opacity-0 transition-all duration-300 ease-out"
    >
        <!-- Modal Content -->
        <h1 class="text-2xl font-bold mb-4 text-center">Welcome!</h1>
        <p class="text-gray-300 text-center mb-4">
            This is a simple tool to practice Amharic touch typing.
        </p>
        <ol class="text-gray-300 text-left list-decimal list-inside mb-6">
            <li>Start typing the Amharic letters you see on screen.</li>
            <li>Focus on accuracy first—speed will come with practice.</li>
            <li>Ignore mistakes (just keep going!).</li>
        </ol>
        <p class="text-gray-300 text-center mb-6">
            The keyboard layout matches <span class="font-bold">Amharic - SIL Ethiopic Power-G keyboard layout</span>. You can learn more about it <a class="underline text-blue-300" href="https://github.com/keymanapp/keyboards/tree/master/release/sil/sil_ethiopic_power_g">here</a>.
        </p>
        <div class="flex justify-center gap-4">
            <!-- Sign Up Button -->
            <a  id="init_signup"
                href="{{ route('signin') }}"
                class="px-6 py-3 bg-lime-500 text-gray-800 font-medium rounded hover:bg-lime-400 transition-all">
                Sign Up
            </a>
            <!-- Explore Button -->
            <button id="explore_as_guest"
                class="px-6 py-3 border border-lime-500 text-lime-500 font-medium rounded hover:bg-gray-600 hover:border-lime-400 transition-all">
                Explore as Guest
            </button>
        </div>
    </div>
</div>

</x-layout>
