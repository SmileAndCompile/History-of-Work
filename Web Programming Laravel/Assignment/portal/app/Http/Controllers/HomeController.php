<?php

namespace App\Http\Controllers;

use Illuminate\Http\Request;

use App\User;


use Illuminate\Support\Facades\Auth;

class HomeController extends Controller
{
    /**
     * Create a new controller instance.
     *
     * @return void
     */
    public function __construct()
    {
        $this->middleware('auth');
    }

    /**
     * Show the application dashboard.
     *
     * @return \Illuminate\Contracts\Support\Renderable
     */
    public function index()
    {
        $user = Auth::user();
        $restaurants = User::whereRaw("role == 'Restaurant'")->paginate(5);
        return view('home')->with('restaurants', $restaurants)->with('user', $user);
    }
}
