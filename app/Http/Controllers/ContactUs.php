<?php

namespace App\Http\Controllers;

use App\Mail\ContactUsSent;
use Illuminate\Http\RedirectResponse;
use Illuminate\Http\Request;
use Illuminate\Support\Facades\Mail;
use ReCaptcha\ReCaptcha;

class ContactUs extends Controller
{
    /**
     * @return RedirectResponse
     */
    public function submitContactForm(Request $request): RedirectResponse {
        $validatedData = $request->validate([
            'name' => 'required|string|max:255',
            'email' => 'required|email',
            'subject' => 'required|string',
            'message' => 'required|string',
            'file' => 'nullable|file|mimes:jpg,jpeg,png,pdf,doc,docx|max:2048',
            'g-recaptcha-response' => 'required'
        ]);

        $recaptcha = new ReCaptcha(config('services.recaptcha.secret'));
        $response = $recaptcha->verify($request->input('g-recaptcha-response'), $request->ip());

        if (!$response->isSuccess()) {
            return back()->withErrors(['captcha' => 'reCAPTCHA verification failed. Please try again.']);
        }

        $filePath = null;
        if ($request->hasFile('file')) {
            $savePath = 'contact-us/' . $validatedData['email'];
            $filePath = $request->file('file')->store($savePath, 'public');
        }

        $emailData = [
            'name' => $validatedData['name'],
            'email' => $validatedData['email'],
            'subject' => $validatedData['subject'],
            'message' => $validatedData['message'],
            'filePath' => $filePath ? asset("storage/$filePath") : null,
        ];

        Mail::to(config('app.admin.email'))->send(new ContactUsSent($emailData));

        return redirect(route("contact-us"))->with('alert.success', 'Thank you for contacting us! We will respond shortly.');
    }
}
