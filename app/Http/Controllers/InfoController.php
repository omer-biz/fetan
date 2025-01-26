<?php

namespace App\Http\Controllers;

use Illuminate\Http\Request;
use Illuminate\Support\Facades\Auth;

class InfoController extends Controller
{
    public function update(Request $req) {
        $req->validate([
            'lessonIdx' => 'int',

            'speed_new' => 'int',
            'speed_old' => 'int',

            'accuracy_new' => 'int',
            'accuracy_old' => 'int',

            'score_new' => 'int',
            'score_old' => 'int',
        ]);

        $user = Auth::user();
        $user->update($req->all());
    }
}
