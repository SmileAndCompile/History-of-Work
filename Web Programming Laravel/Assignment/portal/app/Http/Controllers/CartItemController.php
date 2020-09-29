<?php

namespace App\Http\Controllers;

use Illuminate\Http\Request;

use Illuminate\Validation\Rule;

use Illuminate\Support\Facades\Auth;

use App\Dish;
use App\CartItem;
use App\Order;

use \Datetime;

use Illuminate\Support\Facades\DB;

class CartItemController extends Controller
{

    public function __construct(){
        $this->middleware("role:Consumer", ['only'=> ['index', 'create', 'store', 'show', 'edit', 'update', 'destroy']]);
    }

    /**
     * Display a listing of the resource.
     *
     * @return \Illuminate\Http\Response
     */

    // function to show the cart of the current Consumer (User)
    public function index()
    {
        $user = Auth::user();
        $cart_items = CartItem::whereRaw("cart_items.user_id = $user->id")->join('dishes', 'cart_items.dish_id', '=', 'dishes.id')->select('cart_items.id', 'dishes.image', 'dishes.name', 'dishes.price')->paginate(5);
        $price = CartItem::whereRaw("cart_items.user_id = $user->id")->join('dishes', 'cart_items.dish_id', '=', 'dishes.id')->sum('dishes.price');
        return view('cart')->with('cart_items', $cart_items)->with('price', $price)->with('user', $user);
    }

    /**
     * Store a newly created resource in storage.
     *
     * @param  \Illuminate\Http\Request  $request
     * @return \Illuminate\Http\Response
     */

    // function to add a new item to the consumers cart
    public function store(Request $request)
    {
        $this->validate($request, [
            'user_id' => ['required', 
                        'exists:Users,id'
            ],
            'dish_id' => ['required',
                        'exists:Dishes,id'
            ]
        ]);
        $cart_item = new CartItem();
        $cart_item->user_id = $request->user_id;
        $cart_item->dish_id = $request->dish_id;
        $cart_item->created_at = new DateTime("now");
        $cart_item->updated_at = new DateTime("now");
        $cart_item->save();

        return redirect('/cart');

    }

    /**
     * Display the specified resource.
     *
     * @param  int  $id
     * @return \Illuminate\Http\Response
     */

     // function to purchase all the items of a cart (creating an order for each item), and returning a view showing the customer what they ordered / confirming the purchase
    public function show($id)
    {
        $user = Auth::user();
        $cart_items = CartItem::whereRaw("cart_items.user_id = $user->id")->join('dishes', 'cart_items.dish_id', '=', 'dishes.id')->select('cart_items.id', 'dishes.image', 'dishes.name', 'dishes.user_id', 'dishes.price')->get();
        $order_ids = array();
        foreach($cart_items as $cart_item) { 
            $order = new Order();
            $order->user_id = $user->id;
            $order->dish = $cart_item->name;
            $order->price = $cart_item->price;
            $order->orderDate = new DateTime("now");
            $order->rest_id = $cart_item->user_id;
            $order->created_at = new DateTime("now");
            $order->updated_at = new DateTime("now");
            $order->save();
            array_push($order_ids, $order->id);
            $cart_item->delete();
        }
        $orders = Order::whereIn('id', $order_ids)->get();
        return view('order')->with('orders', $orders)->with('user', $user);
    }

    /**
     * Remove the specified resource from storage.
     *
     * @param  int  $id
     * @return \Illuminate\Http\Response
     */

    // function to delete an cart_item from the cart
    public function destroy($id)
    {
        $item = CartItem::find($id);
        $item->delete();
        return redirect("/cart");
    }
}
