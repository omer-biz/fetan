<!DOCTYPE html>
<html lang="{{ str_replace('_', '-', app()->getLocale()) }}">
    <head>
        <meta charset="utf-8">
        <link href="https://fonts.googleapis.com/css2?family=JetBrains+Mono:wght@400;500;700&display=swap" rel="stylesheet">

        <meta property="og:title" content="Fetan">
        <meta property="og:description" content="Fetan is a typing practice tool for learning the Amharic keyboard layout.">
        <meta property="og:image" content="{{ url('/images/fetan_og_preview.png') }}">
        <meta property="og:url" content="https://fetan.mesnoy.com">
        <meta property="og:type" content="website">

        <meta name="twitter:card" content="summary_large_image">
        <meta name="twitter:title" content="Fetan">
        <meta name="twitter:description" content="Fetan is a typing practice tool for learning the Amharic keyboard layout.">
        <meta name="twitter:image" content="{{ url('/images/fetan_og_preview.png') }}">

        <title>ፈጣን</title>
        @vite(['resources/css/app.css', 'resources/js/app.js'])
        <script>
            function infoToForm(form, info) {
                form.querySelector('input[name="lessonIdx"]').value = info.lessonIdx;

                form.querySelector('input[name="speed.new"]').value = info.metrics.speed.new;
                form.querySelector('input[name="speed.old"]').value = info.metrics.speed.old;

                form.querySelector('input[name="accuracy.new"]').value = info.metrics.accuracy.new;
                form.querySelector('input[name="accuracy.old"]').value = info.metrics.accuracy.old;

                form.querySelector('input[name="score.new"]').value = info.metrics.score.new;
                form.querySelector('input[name="score.old"]').value = info.metrics.score.old;
            }

            function handleFormSubmission(formId, recaptchaAction) {
                const form = document.getElementById(formId);
                form.addEventListener('submit', function (event) {
                    event.preventDefault();

                    if (formId == 'signup-form') {
                        let lessonInfo = localStorage.getItem('lessonInfo');
                        lessonInfo = JSON.parse(lessonInfo);

                        if (lessonInfo != null) {
                            infoToForm(form, lessonInfo);
                        }
                    }

                    grecaptcha.ready(function () {
                        grecaptcha.execute('{{ config('services.recaptcha.key') }}', { action: recaptchaAction }).then(function (token) {
                            form.querySelector('input[name="g-recaptcha-response"]').value = token;
                            form.submit();
                        });
                    });
                });
            }
        </script>
    </head>
    <body class="flex flex-col min-h-screen bg-gray-700">
        @if (session('alert.success'))
            <div id="flash_message" class="cursor-pointer fixed top-4 left-1/2 transform -translate-x-1/2 bg-green-500 text-white px-4 py-2 rounded shadow-lg" onclick="window.flash_message.remove()">
            <span class="hidden bg-green-500 bg-red-500 bg-blue-500 bg-yellow-500"></span>
            {{ session('alert.success') }}
            </div>
        @endif
        <header class="bg-gray-700 text-white py-4">
            <div class="container mx-auto flex justify-between items-center">
                <div class="text-2xl font-bold">
                    <a href="{{ route('home') }}" class="hover:text-gray-300 transition">
                        <x-key>ፈ</x-key>
                        <x-key>ጣ</x-key>
                        <x-key>ን</x-key>
                    </a>
                </div>

                <nav class="flex space-x-6">
                    <a href="{{ route('home') }}" class="text-gray-300 hover:text-white transition">Home</a>
                    <a href="{{ route('scoreboard') }}" class="text-gray-300 hover:text-white transition">Scoreboard</a>
                    <a href="{{ route('faq') }}" class="text-gray-300 hover:text-white transition">FAQ</a>
                    <a href="{{ route('contact-us') }}" class="text-gray-300 hover:text-white transition">Contact</a>
                    @auth
                    <form id="logout_form" method="POST" action="{{ route('auth.logout') }}" class="inline">
                        @csrf
                        @method('DELETE')
                        <button class="text-gray-300 hover:text-white transition">Logout</button>
                    </form>
                    <script>
                     window.logout_form.addEventListener('submit', function(event) {
                         localStorage.setItem('lessonInfo', localStorage.getItem('lessonInfo.bak'));
                     });
                    </script>
                    @endauth
                    @guest
                    <a href="{{ route('signin') }}" class="text-gray-300 hover:text-white transition">SignIn</a>
                    @endguest
                </nav>
            </div>
        </header>

        {{ $slot }}

        <footer class="bg-gray-700 text-white py-8">
            <div class="container mx-auto grid grid-cols-1 md:grid-cols-3 gap-8">
                <div>
                    <h3 class="text-xl font-semibold mb-4">About Fetan</h3>
                    <p class="text-gray-300">
                        Fetan is a typing practice tool for learning the Amharic keyboard layout. Improve your speed and accuracy while mastering the language.
                    </p>
                </div>

                <div>
                    <h3 class="text-xl font-semibold mb-4">Quick Links</h3>
                    <ul>
                        <li><a href="{{ route('home') }}" class="text-gray-300 hover:text-white transition">Home</a></li>
                        <li><a href="{{ route('scoreboard') }}" class="text-gray-300 hover:text-white transition">Scoreboard</a></li>
                        <li><a href="{{ route('faq') }}" class="text-gray-300 hover:text-white transition">FAQ</a></li>
                        <li><a href="{{ route('contact-us') }}" class="text-gray-300 hover:text-white transition">Contact</a></li>
                    </ul>
                </div>

                <div>
                    <h3 class="text-xl font-semibold mb-4">Follow Us</h3>
                    <div class="flex space-x-4">
                        <a href="https://t.me/fetan_mesnoy" class="">
                            <svg class="h-8" viewBox="0 0 32 32" fill="none" xmlns="http://www.w3.org/2000/svg"><g id="SVGRepo_bgCarrier" stroke-width="0"></g><g id="SVGRepo_tracerCarrier" stroke-linecap="round" stroke-linejoin="round"></g><g id="SVGRepo_iconCarrier"> <circle cx="16" cy="16" r="14" fill="url(#paint0_linear_87_7225)"></circle> <path d="M22.9866 10.2088C23.1112 9.40332 22.3454 8.76755 21.6292 9.082L7.36482 15.3448C6.85123 15.5703 6.8888 16.3483 7.42147 16.5179L10.3631 17.4547C10.9246 17.6335 11.5325 17.541 12.0228 17.2023L18.655 12.6203C18.855 12.4821 19.073 12.7665 18.9021 12.9426L14.1281 17.8646C13.665 18.3421 13.7569 19.1512 14.314 19.5005L19.659 22.8523C20.2585 23.2282 21.0297 22.8506 21.1418 22.1261L22.9866 10.2088Z" fill="white"></path> <defs> <linearGradient id="paint0_linear_87_7225" x1="16" y1="2" x2="16" y2="30" gradientUnits="userSpaceOnUse"> <stop stop-color="#37BBFE"></stop> <stop offset="1" stop-color="#007DBB"></stop> </linearGradient> </defs> </g></svg>
                        </a>
                    </div>
                </div>
            </div>

            <div class="mt-8 border-t border-gray-600 pt-4 text-center text-gray-400 text-sm">
                © 2025 Fetan. All rights reserved.
            </div>
        </footer>
    </body>
</html>
