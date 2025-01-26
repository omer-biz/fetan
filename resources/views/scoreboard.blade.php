<x-layout>
<main class="flex-grow bg-gray-700 text-white py-12 px-4 h-full">
    <div class="container mx-auto max-w-4xl">
        <h1 class="text-4xl font-bold text-center mb-6">Scoreboard</h1>
        <p class="text-center text-gray-300 mb-8">
            Sign up to see your name on the leaderboard. Finish all <a class="underline text-blue-400" href="{{ route('faq') }}/#key-groups">key groups</a> and complete all 33 keys to qualify!
        </p>

        <table class="w-full text-left text-gray-200">
            <thead class="text-gray-300 border-b border-gray-600">
                <tr>
                    <th class="py-2 px-3">Rank</th>
                    <th class="py-2 px-3">Username</th>
                    <th class="py-2 px-3">Speed</th>
                    <th class="py-2 px-3">Accuracy</th>
                    <th class="py-2 px-3 text-right">Score</th>
                </tr>
            </thead>
            <tbody class="divide-y divide-gray-600">
        @if( ! $topScorers->isEmpty())
            @foreach($topScorers as $top)
                <tr class="hover:bg-gray-600">
                    <td class="py-2 px-3">{{ $loop->iteration }}</td>
                    <td class="py-2 px-3">{{ $top->username }}</td>
                    <td class="py-2 px-3">{{ $top->speed_new }}wpm</td>
                    <td class="py-2 px-3">{{ $top->accuracy_new }}%</td>
                    <td class="py-2 px-3 text-right">{{ $top->score_new }}</td>
                </tr>
            @endforeach
        @else
            <tr>
                <td colspan="5" class="py-12 text-center text-gray-400">
                    <div>
                        <p class="text-lg font-medium mb-4">No scores yet!</p>
                        <p class="mb-6">Sign up, complete all key groups, and compete to see your name on the leaderboard.</p>
                        <a href="{{ route('signin') }}" class="inline-block bg-lime-500 text-gray-800 font-semibold py-2 px-4 rounded hover:bg-lime-400">
                            Get Started
                        </a>
                    </div>
                </td>
            </tr>
        @endif
            </tbody>
        </table>


    </div>
</main>
</x-layout>
