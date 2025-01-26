<?php

use App\Http\Controllers\AuthController;
use App\Http\Controllers\ContactUs;
use App\Http\Controllers\InfoController;
use App\Http\Controllers\ScoreboardController;
use Illuminate\Support\Facades\Route;

Route::get('/', fn () => view('index'))->name('home');
Route::get('/faq', fn() => view('faq'))->name('faq');
Route::get('/contact-us', fn() => view('contact-us'))->name('contact-us');
Route::get('/signin', fn() => view('signin'))->name('signin');

Route::get('/scoreboard', [ScoreboardController::class, "leaderBoard"])->name('scoreboard');
Route::middleware(['captcha'])->group(function () {
    Route::post('/signup', [AuthController::class, 'signup'])->name('auth.signup');
    Route::post('/signin', [AuthController::class, 'signin'])->name('auth.signin');
    Route::post('/contact-us', [ContactUs::class, 'submitContactForm'])->name('contact-us.post');
});

Route::middleware(['auth'])->group(function () {
    Route::delete('/logout', [AuthController::class, 'logout'])->name('auth.logout');
    Route::post('/info/update', [InfoController::class, 'update'])->name('info.update');
});
