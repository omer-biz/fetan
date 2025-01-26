<!DOCTYPE html>
<html lang="{{ str_replace('_', '-', app()->getLocale()) }}">
    <head>
        <meta charset="utf-8">
        <link href="https://fonts.googleapis.com/css2?family=JetBrains+Mono:wght@400;500;700&display=swap" rel="stylesheet">

        <title>Fetan</title>
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
                <!-- Logo -->
                <div class="text-2xl font-bold">
                    <a href="{{ route('home') }}" class="hover:text-gray-300 transition">Fetan</a>
                </div>

                <!-- Navigation -->
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
                         localStorage.setItem('lessonInfo', '');
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
                <!-- About Section -->
                <div>
                    <h3 class="text-xl font-semibold mb-4">About Fetan</h3>
                    <p class="text-gray-300">
                        Fetan is a typing practice tool for learning the Amharic keyboard layout. Improve your speed and accuracy while mastering the language.
                    </p>
                </div>

                <!-- Quick Links -->
                <div>
                    <h3 class="text-xl font-semibold mb-4">Quick Links</h3>
                    <ul>
                        <li><a href="{{ route('home') }}" class="text-gray-300 hover:text-white transition">Home</a></li>
                        <li><a href="{{ route('scoreboard') }}" class="text-gray-300 hover:text-white transition">Scoreboard</a></li>
                        <li><a href="{{ route('faq') }}" class="text-gray-300 hover:text-white transition">FAQ</a></li>
                        <li><a href="{{ route('contact-us') }}" class="text-gray-300 hover:text-white transition">Contact</a></li>
                    </ul>
                </div>

                <!-- Social Media -->
                <div>
                    <h3 class="text-xl font-semibold mb-4">Follow Us</h3>
                    <div class="flex space-x-4">
                        <a href="#" class="text-gray-300 hover:text-white transition">
                            <svg xmlns="http://www.w3.org/2000/svg" class="h-6 w-6" fill="currentColor" viewBox="0 0 24 24">
                                <path d="M24 4.6a10.13 10.13 0 01-2.83.78 4.93 4.93 0 002.17-2.71 9.72 9.72 0 01-3.1 1.19A4.92 4.92 0 0016.75 3c-2.73 0-4.94 2.21-4.94 4.93 0 .38.04.76.13 1.12A13.94 13.94 0 011.67 3.15a4.93 4.93 0 001.52 6.57 4.93 4.93 0 01-2.23-.62v.06c0 2.4 1.71 4.42 3.98 4.88a5.02 5.02 0 01-2.2.08 4.92 4.92 0 004.6 3.41A9.89 9.89 0 010 21.54a14 14 0 007.55 2.21c9.05 0 14-7.5 14-14v-.64A10.02 10.02 0 0024 4.6z" />
                            </svg>
                        </a>
                        <a href="#" class="text-gray-300 hover:text-white transition">
                            <svg xmlns="http://www.w3.org/2000/svg" class="h-6 w-6" fill="currentColor" viewBox="0 0 24 24">
                                <path d="M9 3v18c-1.104-.005-2.211-.25-3.197-.729C4.243 19.566 3.109 18.61 2.25 17.5 1.39 16.39 1.059 15.119.941 13.75H4c.077.394.234.773.463 1.118.228.344.521.651.868.905.346.253.739.452 1.158.587.42.134.86.198 1.302.193v-4.5H3.579L3.568 10.5h3.422v-3c0-1.464.396-2.782 1.142-3.884C8.879 2.513 10.067 2 11.292 2h3.708V5h-2.708c-.498 0-.978.212-1.293.586C10.978 5.96 10.786 6.44 10.786 7v2h4.358l-.641 3H10.786V18c2.761 0 5.306-.986 7.25-2.5 1.944-1.514 3.25-3.635 3.735-5.895.482-2.26.482-4.62 0-6.88C21.286 1.54 19.98-.086 18.036-1.6 16.092-3.114 13.547-4 10.786-4H8z" />
                            </svg>
                        </a>
                    </div>
                </div>
            </div>

            <!-- Bottom Section -->
            <div class="mt-8 border-t border-gray-600 pt-4 text-center text-gray-400 text-sm">
                © 2025 Fetan. All rights reserved.
            </div>
        </footer>
    </body>
</html>
