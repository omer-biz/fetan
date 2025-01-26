<x-layout>
<script src="https://www.google.com/recaptcha/api.js?render={{ config('services.recaptcha.key') }}"></script>
<main class="bg-gray-700 text-white py-12 px-4">
    <div class="container mx-auto max-w-3xl">
        <h2 class="text-3xl font-bold text-center mb-8">Contact Us</h2>
        <p class="text-lg text-center text-gray-300 mb-6">Have questions or feedback? We'd love to hear from you!</p>

        <div class="bg-gray-800 p-8 rounded-lg shadow-lg">
            <form action="{{ route('contact-us.post') }}" method="POST" enctype="multipart/form-data">
                @csrf
                <div class="space-y-6">
                    <!-- Name Field -->
                    <div>
                        <label for="name" class="block text-sm font-semibold text-gray-200">Full Name</label>
                        <input type="text" id="name" name="name" required class="w-full px-4 py-3 mt-2 bg-gray-900 text-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-lime-400">
                    </div>

                    <!-- Email Field -->
                    <div>
                        <label for="email" class="block text-sm font-semibold text-gray-200">Email Address</label>
                        <input type="email" id="email" name="email" required class="w-full px-4 py-3 mt-2 bg-gray-900 text-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-lime-400">
                    </div>

                    <div>
                    <label for="subject" class="block text-sm font-semibold text-gray-200">Subject</label>
                    <select id="subject" name="subject"
                        class="w-full px-4 py-3 mt-2 bg-gray-900 text-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-lime-400">
                        <option value="feature">Feature Request</option>
                        <option value="general">General Inquiry</option>
                        <option value="feedback">Feedback/Suggestions</option>
                        <option value="bug">Bug Report</option>
                        <option value="other">Other</option>
                    </select>
                    </div>

                    <div>
                    <label for="file" class="block text-sm font-semibold text-gray-200">Attachment (Optional)</label>
                    <input type="file" id="file" name="file"
                        class="w-full px-4 py-3 mt-2 bg-gray-900 text-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-lime-400">
                    </div>

                    <!-- Message Field -->
                    <div>
                        <label for="message" class="block text-sm font-semibold text-gray-200">Your Message</label>
                        <textarea id="message" name="message" rows="4" required class="w-full px-4 py-3 mt-2 bg-gray-900 text-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-lime-400"></textarea>
                    </div>

                    <input type="hidden" name="g-recaptcha-response" id="g-recaptcha-response">

                    <!-- Submit Button -->
                    <div class="text-center">
                        <button type="submit" class="px-6 py-3 bg-lime-400 text-gray-900 font-semibold rounded-lg hover:bg-lime-500 transition duration-200">Send Message</button>
                    </div>
                </div>
            </form>
        </div>
    </div>
</main>

</x-layout>
