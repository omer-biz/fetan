<?php

namespace App\Http\Controllers;

use App\Models\User;
use Illuminate\Contracts\View\View;
use Illuminate\Http\Request;

class ScoreboardController extends Controller
{
    /**
     * @return View
     */
    public function leaderBoard(Request $req): View {
        $topScorers = User::query()
            ->orderBy('score_new', 'desc')
            ->take(20)
            ->where('lessonIdx', 4)
            ->get();

        return view('scoreboard', [
            'topScorers' => $topScorers
        ]);
    }
}
