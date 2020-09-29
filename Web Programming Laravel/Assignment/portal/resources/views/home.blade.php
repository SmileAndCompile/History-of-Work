<!-- Home page of the website. All available restaurants are shown -->

@extends('layouts.app')

@section('title')
    Home Page
@endsection

@section('content')
    <div class = "container">
        <div id = "content">
            <h1>Restaurants: </h1>

            <!-- display the details of each of the available Restaurants on the website -->
            @foreach ($restaurants as $restaurant)
            <a href="{{url("restaurant/$restaurant->id")}}">
                <div id = "media">
                    <table>
                        <col width = "150">
                        <col width = "150">
                        <tr>
                            <th>Name: </th>
                            <th>Address: </th>
                        </tr>
                        <tr>
                            <td><h4>{{ $restaurant->name }}</h4></td>
                            <td>{{ $restaurant->address}}</td>
                        </tr>
                    </table>
                </div>
            </a>
            @endforeach
            <div class = "pagination">
                {{ $restaurants->links() }}
            </div>
        </div>
    </div>
@endsection