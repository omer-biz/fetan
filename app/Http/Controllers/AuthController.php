<?php

namespace App\Http\Controllers;

use App\Models\User;
use Illuminate\Http\RedirectResponse;
use Illuminate\Http\Request;
use Illuminate\Support\Facades\Auth;
use Illuminate\Support\Facades\Hash;

class AuthController extends Controller
{
    /**
     * @return RedirectResponse|<missing>
     */
    public function signup(Request $req) {
        $req->validate([
            'username' => 'required|string|max:255|unique:users,username',
            'password' => 'required|string|min:8|confirmed',

            'lessonIdx' => 'int',

            'speed_new' => 'int',
            'speed_old' => 'int',

            'accuracy_new' => 'int',
            'accuracy_old' => 'int',

            'score_new' => 'int',
            'score_old' => 'int',

        ]);

        $data = $req->all();
        $data['password'] = Hash::make($req->input('password'));

        $user = User::create($data);

        Auth::login($user);

        return redirect()->route('home')->with('alert.success', 'Account created successfully!');
    }
    /**
     * @return <missing>|RedirectResponse
     */
    public function signin(Request $req) {
        $req->validate([
            'username' => 'required|string',
            'password' => 'required|string',
        ]);

        if (Auth::attempt($req->only('username', 'password'))) {
            $req->session()->regenerate();

            return redirect()->route('home')->with('alert.success', 'Welcome back!');
        }

        return back()->withErrors([
            'username' => 'The provided credentials do not match our records.,'
        ])->withInput($req->except('password'));
    }


    public function logout() {
        Auth::logout();
        return redirect()->route('home')->with('alert.success', 'Logged out successfully');
    }
}
