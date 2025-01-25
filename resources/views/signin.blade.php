<x-layout>
<main class="flex-grow bg-gray-700 text-white flex flex-col items-center py-12 px-4">
    <!-- Welcome Message -->
    <div class="max-w-2xl text-center mb-8">
        <h1 class="text-3xl font-semibold mb-4">Join Us and Improve Your Typing Skills</h1>
        <p class="text-gray-300">
            Sign up to track your progress across computers, and secure a spot on the global leaderboard.
            Already have an account? Sign in below!
        </p>
    </div>

    <!-- Tab Controls -->
    <div class="flex justify-center space-x-4 mb-6">
        <button
            id="signin-tab"
            class="text-lg font-semibold py-2 border-b-2 transition-all"
            onclick="switchTab('signin')">
            Sign In
        </button>
        <button
            id="signup-tab"
            class="text-lg font-semibold py-2 border-b-2 transition-all"
            onclick="switchTab('signup')">
            Sign Up
        </button>
    </div>

    <!-- Forms Container -->
    <div class="w-full max-w-lg">
        <!-- Sign In Form -->
        <form id="signin-form" class="space-y-6">
            <div>
                <label for="username" class="block text-sm font-medium">Username</label>
                <input
                    id="username"
                    name="username"
                    type="text"
                    class="w-full mt-1 p-3 bg-gray-600 text-white rounded focus:outline-none focus:ring-2 focus:ring-lime-500">
            </div>
            <div>
                <label for="password" class="block text-sm font-medium">Password</label>
                <input
                    id="password"
                    name="password"
                    type="password"
                    class="w-full mt-1 p-3 bg-gray-600 text-white rounded focus:outline-none focus:ring-2 focus:ring-lime-500">
            </div>

            <!-- Google reCAPTCHA -->
            <div class="mb-4">
                <div class="g-recaptcha" data-sitekey="{{ config('services.recaptcha.key') }}"></div>
                @error('g-recaptcha-response')
                <p class="mt-2 text-sm text-red-600">{{ $message }}</p>
                @enderror
            </div>
            <button
                type="submit"
                class="w-full py-3 bg-lime-500 text-gray-800 font-medium rounded hover:bg-lime-400 transition-all">
                Sign In
            </button>
        </form>

        <!-- Sign Up Form -->
        <form id="signup-form" class="space-y-6 hidden">
            <div>
                <label for="susername" class="block text-sm font-medium">Username</label>
                <input
                    id="username"
                    type="text"
                    class="w-full mt-1 p-3 bg-gray-600 text-white rounded focus:outline-none focus:ring-2 focus:ring-lime-500">
            </div>
            <div>
                <label for="password" class="block text-sm font-medium">Password</label>
                <input
                    id="password"
                    type="password"
                    class="w-full mt-1 p-3 bg-gray-600 text-white rounded focus:outline-none focus:ring-2 focus:ring-lime-500">
            </div>

            <!-- Google reCAPTCHA -->
            <div class="mb-4">
                <div class="g-recaptcha" data-sitekey="{{ config('services.recaptcha.key') }}"></div>
                @error('g-recaptcha-response')
                <p class="mt-2 text-sm text-red-600">{{ $message }}</p>
                @enderror
            </div>

            <button
                type="submit"
                class="w-full py-3 bg-lime-500 text-gray-800 font-medium rounded hover:bg-lime-400 transition-all">
                Sign Up
            </button>
        </form>
    </div>
</main>

<script src="https://www.google.com/recaptcha/api.js" async defer></script>
<script>
    function switchTab(tab) {
        // Update the tab styling
        document.getElementById('signin-tab').classList.toggle('border-lime-500', tab === 'signin');
        document.getElementById('signin-tab').classList.toggle('border-gray-600', tab !== 'signin');
        document.getElementById('signup-tab').classList.toggle('border-lime-500', tab === 'signup');
        document.getElementById('signup-tab').classList.toggle('border-gray-600', tab !== 'signup');

        // Toggle form visibility
        document.getElementById('signin-form').classList.toggle('hidden', tab !== 'signin');
        document.getElementById('signup-form').classList.toggle('hidden', tab !== 'signup');
    }

    // Initialize default active tab
    switchTab('signin');
</script>

</x-layout>
