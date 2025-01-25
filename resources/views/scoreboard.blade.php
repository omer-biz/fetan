<x-layout>
<main class="flex-grow bg-gray-700 text-white py-12 px-4 h-full">
    <div class="container mx-auto max-w-4xl">
        <h1 class="text-4xl font-bold text-center mb-6">Scoreboard</h1>
        <p class="text-center text-gray-300 mb-8">
            Sign up to see your name on the leaderboard. Finish all key groups and complete all 33 keys to qualify!
        </p>

        <!-- Leaderboard Table -->
        <table class="w-full text-left text-gray-200">
            <thead class="text-gray-300 border-b border-gray-600">
                <tr>
                    <th class="py-2 px-3">Rank</th>
                    <th class="py-2 px-3">Username</th>
                    <th class="py-2 px-3 text-right">Score</th>
                </tr>
            </thead>
            <tbody class="divide-y divide-gray-600">
                <!-- Example Leader -->
                <tr class="hover:bg-gray-600">
                    <td class="py-2 px-3">1</td>
                    <td class="py-2 px-3">KeyMaster</td>
                    <td class="py-2 px-3 text-right">99.9 WPM</td>
                </tr>
                <tr class="hover:bg-gray-600">
                    <td class="py-2 px-3">2</td>
                    <td class="py-2 px-3">FastTyper</td>
                    <td class="py-2 px-3 text-right">98.3 WPM</td>
                </tr>
                <tr class="hover:bg-gray-600">
                    <td class="py-2 px-3">3</td>
                    <td class="py-2 px-3">ProAmharic</td>
                    <td class="py-2 px-3 text-right">96.5 WPM</td>
                </tr>
            </tbody>
        </table>

        <!-- Empty State -->
        <!-- <tbody class="divide-y divide-gray-600">
             <tr>
             <td colspan="3" class="py-12 text-center text-gray-400">
             <div>
             <p class="text-lg font-medium mb-4">No scores yet!</p>
             <p class="mb-6">Sign up, complete all key groups, and compete to see your name on the leaderboard.</p>
             <a href="/sign-up" class="inline-block bg-lime-500 text-gray-800 font-semibold py-2 px-4 rounded hover:bg-lime-400">
             Get Started
             </a>
             </div>
             </td>
             </tr>
             </tbody> -->


    </div>
</main>
</x-layout>
