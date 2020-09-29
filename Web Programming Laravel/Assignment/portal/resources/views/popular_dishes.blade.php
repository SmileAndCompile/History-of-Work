<!-- view to show most popular dishes from the past 30 days (most ordered) -->

@extends('layouts.app')

@section('title')
    Most Popular Dishes
@endsection

@section('content')
    <div class = "container">
        <div id="content">
            <h1>Most Popular Dishes in Past 30 Days: </h1>
            <!-- display the details of the 5 most popular dishes -->
            @foreach($dishes as $dish)
                <div id="media">
                    <table>
                        <col width="100">
                        <col width="100">
                        <col width="100">
                        @if(Auth::user() && Auth::user()->role == "Consumer")
                            <col width="100">
                        @endif
                        <tr>
                            <th>Photo: </th>
                            <th>Name: </th>
                            <th>Price: </th>
                            @if(Auth::user() && Auth::user()->role == "Consumer")
                                <th>Options: </th>
                            @endif
                        </tr>
                        <tr>
                            <td><img src="{{ asset("/images/$dish->image")}}" alt="Photo" width=100 height=100></td>
                            <td>{{ $dish->name }}</td>
                            <td>$ {{ $dish->price }}</td>

                            <!-- if the currently signed in user is a Consumer - allow them to purchase each dish -->
                            @if(Auth::user() && Auth::user()->role == "Consumer")
                                <td>
                                    <form action= "{{url("/order")}}" method="Post">
                                        {{ csrf_field() }}
                                        <input type="hidden" value="{{ $dish->name }}" name="dish">
                                        <input type="hidden" value="{{ $dish->price }}" name="price">
                                        <input type="hidden" value="{{ $dish->user_id }}" name="rest_id">
                                        <input type="submit" value="Purchase" id="purchase" class="search">
                                    </form>
                                </td>
                            @endif
                        </tr>
                    </table>
                </div>
            @endforeach
        </div>
    </div>
@endsection