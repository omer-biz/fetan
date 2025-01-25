<?php

use App\Http\Controllers\ContactUs;
use Illuminate\Support\Facades\Route;

Route::get('/', fn () => view('index'))->name('home');

Route::get('/faq', fn() => view('faq'))->name('faq');

Route::get('/scoreboard', fn() => view('scoreboard'))->name('scoreboard');

Route::get('/contact-us', fn() => view('contact-us'))->name('contact-us');
Route::post('/contact-us', [ContactUs::class, 'submitContactForm'])->name('contact-us.post');
