<?php

namespace App\Http\Controllers;

use Illuminate\Http\Request;

use Illuminate\Validation\Rule;

use Illuminate\Support\Facades\Auth;

use App\Order;

use Carbon\Carbon;

use \Datetime;

class StatisticsController extends Controller
{

    public function __construct(){
        $this->middleware("statistics", ['only'=> ['index', 'create', 'store', 'show', 'edit', 'update', 'destroy']]);
    }

    /**
     * Display a listing of the resource.
     *
     * @return \Illuminate\Http\Response
     */

     // function to show the sales statistics of a Restaurant (User)
    public function index()
    {
        $user = Auth::user();
        $weekly_sales = array();
        $total_sales = Order::whereRaw("orders.rest_id = $user->id")->sum('orders.price');
            
        foreach (range(0,77,7) as $i) {
            $end = $i + 7;
            $week = array(Carbon::now()->subDays($end)->format('Y-m-d'), Carbon::now()->subDays($i)->format('Y-m-d'), Order::whereRaw("(orders.rest_id = $user->id) AND orderDate BETWEEN ? AND ?", array(Carbon::now()->subDays($end)->format('Y-m-d H:i:s'), Carbon::now()->subDays($i)->format('Y-m-d H:i:s')))->sum('orders.price'));
            array_push($weekly_sales, $week);
        }
        return view('statistics')->with('weekly_sales', $weekly_sales)->with('total_sales', $total_sales)->with('user', $user);
    }
}
