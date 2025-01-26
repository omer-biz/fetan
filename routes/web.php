<?php

use App\Http\Controllers\AuthController;
use App\Http\Controllers\ContactUs;
use Illuminate\Support\Facades\Route;

Route::get('/', fn () => view('index'))->name('home');
Route::get('/faq', fn() => view('faq'))->name('faq');
Route::get('/scoreboard', fn() => view('scoreboard'))->name('scoreboard');
Route::get('/contact-us', fn() => view('contact-us'))->name('contact-us');
Route::get('/signin', fn() => view('signin'))->name('signin');

Route::middleware(['captcha'])->group(function () {
    Route::post('/signup', [AuthController::class, 'signup'])->name('auth.signup');
    Route::post('/signin', [AuthController::class, 'signin'])->name('auth.signin');
    Route::post('/contact-us', [ContactUs::class, 'submitContactForm'])->name('contact-us.post');
});

Route::delete('/logout', [AuthController::class, 'logout'])->name('auth.logout');
