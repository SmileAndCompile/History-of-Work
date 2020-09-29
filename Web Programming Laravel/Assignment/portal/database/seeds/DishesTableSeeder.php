<?php

use Illuminate\Database\Seeder;

class DishesTableSeeder extends Seeder
{
    /**
     * Run the database seeds.
     *
     * @return void
     */

    // function to seed Dishes into the Dishes table of the database
    public function run()
    {
        DB::table('dishes')->insert([
            'name' => 'Chicken Burger',
            'price' => '10.50',
            'image' => '991936215.png',
            'user_id' => '4',
            'created_at' => DB::raw('CURRENT_TIMESTAMP'),
            'updated_at' => DB::raw('CURRENT_TIMESTAMP')
        ]); 

        DB::table('dishes')->insert([
            'name' => 'Beef Burger',
            'price' => '9.50',
            'image' => '991936215.png',
            'user_id' => '4',
            'created_at' => DB::raw('CURRENT_TIMESTAMP'),
            'updated_at' => DB::raw('CURRENT_TIMESTAMP')
        ]); 

        DB::table('dishes')->insert([
            'name' => 'Vegan Burger',
            'price' => '12.50',
            'image' => '991936215.png',
            'user_id' => '4',
            'created_at' => DB::raw('CURRENT_TIMESTAMP'),
            'updated_at' => DB::raw('CURRENT_TIMESTAMP')
        ]); 

        DB::table('dishes')->insert([
            'name' => 'Reef Burger',
            'price' => '11.00',
            'image' => '991936215.png',
            'user_id' => '4',
            'created_at' => DB::raw('CURRENT_TIMESTAMP'),
            'updated_at' => DB::raw('CURRENT_TIMESTAMP')
        ]); 

        DB::table('dishes')->insert([
            'name' => 'Sweet Potato Fries',
            'price' => '5.50',
            'image' => '991936215.png',
            'user_id' => '4',
            'created_at' => DB::raw('CURRENT_TIMESTAMP'),
            'updated_at' => DB::raw('CURRENT_TIMESTAMP')
        ]); 

        DB::table('dishes')->insert([
            'name' => 'Pork Belly Burger',
            'price' => '9.00',
            'image' => '991936215.png',
            'user_id' => '4',
            'created_at' => DB::raw('CURRENT_TIMESTAMP'),
            'updated_at' => DB::raw('CURRENT_TIMESTAMP')
        ]); 

        DB::table('dishes')->insert([
            'name' => 'Cheeseburger',
            'price' => '3.50',
            'image' => '991936215.png',
            'user_id' => '3',
            'created_at' => DB::raw('CURRENT_TIMESTAMP'),
            'updated_at' => DB::raw('CURRENT_TIMESTAMP')
        ]); 

        DB::table('dishes')->insert([
            'name' => 'Big Mac',
            'price' => '6.50',
            'image' => '991936215.png',
            'user_id' => '3',
            'created_at' => DB::raw('CURRENT_TIMESTAMP'),
            'updated_at' => DB::raw('CURRENT_TIMESTAMP')
        ]); 

        DB::table('dishes')->insert([
            'name' => 'McChicken',
            'price' => '6.00',
            'image' => '991936215.png',
            'user_id' => '3',
            'created_at' => DB::raw('CURRENT_TIMESTAMP'),
            'updated_at' => DB::raw('CURRENT_TIMESTAMP')
        ]); 

        DB::table('dishes')->insert([
            'name' => 'McNuggets',
            'price' => '7.50',
            'image' => '991936215.png',
            'user_id' => '3',
            'created_at' => DB::raw('CURRENT_TIMESTAMP'),
            'updated_at' => DB::raw('CURRENT_TIMESTAMP')
        ]); 

        DB::table('dishes')->insert([
            'name' => 'Hamburger',
            'price' => '3.00',
            'image' => '991936215.png',
            'user_id' => '3',
            'created_at' => DB::raw('CURRENT_TIMESTAMP'),
            'updated_at' => DB::raw('CURRENT_TIMESTAMP')
        ]); 

        DB::table('dishes')->insert([
            'name' => 'Quarter Pounder',
            'price' => '5.00',
            'image' => '991936215.png',
            'user_id' => '3',
            'created_at' => DB::raw('CURRENT_TIMESTAMP'),
            'updated_at' => DB::raw('CURRENT_TIMESTAMP')
        ]); 

        DB::table('dishes')->insert([
            'name' => 'Happy Meal',
            'price' => '5.00',
            'image' => '991936215.png',
            'user_id' => '3',
            'created_at' => DB::raw('CURRENT_TIMESTAMP'),
            'updated_at' => DB::raw('CURRENT_TIMESTAMP')
        ]); 
    }
}