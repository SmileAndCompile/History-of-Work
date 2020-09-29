<!-- view for showing the available Dishes of a Restaurant. Only Consumers (Users) will be able to purchase a dish -->

@extends('layouts.app')

@section('title')
    Dishes
@endsection

@section('content')
    <div id = content>
        <h1>{{$restaurant->name}}</h1>
        <div class = "container">
            <div id="content">
                @if(count($dishes) <= 0)
                    <h4>This Restaurant currently has no Dishes</h4>
                @else
                    <!-- display each dish for the current restaurant -->
                    @foreach ($dishes as $dish)
                        <div id = "media">
                            <table>
                                <col width="100">
                                <col width="100">
                                <col width="100">
                                @if(Auth::user()) 
                                    @if((Auth::user()->id == $dish->user_id) || (Auth::user()->role == "Consumer"))
                                        <col width="100">
                                    @endif
                                @endif
                                <tr>
                                    <th>Photo: </th>
                                    <th>Name: </th>
                                    <th>Price: </th>
                                    @if(Auth::user()) 
                                        @if(Auth::user()->id == $dish->user_id)
                                            <th>Options: </th>
                                        @elseif(Auth::user()->role == "Consumer")
                                            <th>Purchase: </th>
                                        @endif
                                    @endif
                                </tr>
                                <tr>
                                    <td><img src="{{ asset("/images/$dish->image")}}" alt="Photo" width=100 height=100></td>
                                    <td>{{$dish->name}}</td>
                                    <td>$ {{$dish->price}}</td>
                                    @if(Auth::user())
                                        <!-- provide purchase / add item to cart options if User is a Consumer -->
                                        @if(Auth::user()->role == "Consumer")
                                            <td>
                                                <form action= "{{url("/order")}}" method="Post">
                                                    {{ csrf_field() }}
                                                    <input type="hidden" value="{{ $dish->name }}" name="dish">
                                                    <input type="hidden" value="{{ $dish->price }}" name="price">
                                                    <input type="hidden" value="{{ $dish->user_id }}" name="rest_id">
                                                    <input type="submit" value="Purchase" id="purchase" class="search" width=75px>
                                                </form>

                                                <form action= "{{url("/cart")}}" method="Post">
                                                    {{ csrf_field() }}
                                                    <input type="hidden" value="{{ $dish->id }}" name="dish_id">
                                                    <input type="hidden" value="{{ $user->id }}" name="user_id">
                                                    <input type="submit" value="Add to Cart" id="cart" class="search" width=75px>
                                                </form>
                                            </td>
                                        <!-- provide edit / delete options if User is a Restaurant AND they own the dish -->
                                        @elseif((Auth::user()->id == $dish->user_id) && (Auth::user()->approval == TRUE))
                                            <td>
                                                <form action='{{url("dish/$dish->id/edit")}}' method="get">
                                                    {{ csrf_field() }}
                                                    <input type="submit" value="Update" id="update" class="search">
                                                </form>   
                                                <form action="{{url("dish/$dish->id")}}" method="Post"> 
                                                    {{ csrf_field() }}
                                                    {{ method_field('DELETE') }} 
                                                    <input type="submit" value="Delete"> 
                                                </form> 
                                            </td>
                                        @elseif((Auth::user()->id == $dish->user_id) && (Auth::user()->approval == FALSE))
                                            <td>
                                                <label>N/A</label>
                                            </td>
                                        @endif
                                    @endif
                                </tr>
                            </table>
                        </div>
                    @endforeach
                @endif
                @if(Auth::user())
                    <!-- provide option to add a new Dish if the currently logged in User owns the current Restaurant being viewed -->
                    @if((Auth::user()->id == $restaurant->id) && Auth::user()->approval == TRUE)
                        <form action="{{url("dish/create")}}" method="GET">
                            {{ csrf_field() }}
                            <input type="submit" value="Add Item" id="add" class="add">
                        </form> 
                    @endif
                @endif
                <br>
                <div class = "pagination">
                    {{ $dishes->links() }}
                </div>
            </div>
        </div>
    </div>
@endsection