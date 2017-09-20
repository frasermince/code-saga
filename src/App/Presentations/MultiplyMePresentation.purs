module App.Presentations.MultiplyMe where

import Prelude
import App.Prelude
import App.State (SlideData(..), BeforeOrAfter(..))

newFactory ="\
\FactoryGirl.define do\n\
\  factory :donation do\n\
\    organization_id {Organization.first.present? ? Organization.first.id : create(:organization).id}\n\
\    user_id {User.first.present? ? User.first.id : create(:user).id}\n\
\    amount 300\n\
\\n\
\    factory :stripe_donation do\n\
\      is_paid true\n\
\      is_subscription true\n\
\      factory :unpaid_stripe_donation do\n\
\        is_paid false\n\
\        is_subscription false\n\
\      end\n\
\      amount 10000\n\
\      user_id {create(:stripe_user).id}\n\
\    end\n\
\  end\n\
\end"

factories="\
\FactoryGirl.define do\n\
\  factory :donation do\n\
\    organization_id {Organization.first.present? ? Organization.first.id : create(:organization).id}\n\
\    user_id {User.first.present? ? User.first.id : create(:user).id}\n\
\    amount 300\n\
\\n\
\    factory :stripe_donation do\n\
\      is_paid true\n\
\      is_subscription true\n\
\      factory :unpaid_stripe_donation do\n\
\        is_paid false\n\
\        is_subscription false\n\
\      end\n\
\      amount 10000\n\
\      user_id {create(:stripe_user).id}\n\
\    end\n\
\\n\
\    factory :first_new_user_donation do\n\
\      id 6\n\
\      amount 1000\n\
\      user_id {create(:second_user).id}\n\
\      parent_id 1\n\
\      factory :second_new_user_donation do\n\
\        user_id {create(:third_user).id}\n\
\        id 3\n\
\        parent_id 6\n\
\      end\n\
\      factory :third_new_user_donation do\n\
\        user_id {create(:fourth_user).id}\n\
\        id 4\n\
\        parent_id 3\n\
\      end\n\
\    end\n\
\\n\
\    factory :subscription_donation do\n\
\      is_subscription true\n\
\      factory :nonsubscription_donation do\n\
\        is_subscription false\n\
\      end\n\
\    end\n\
\\n\
\    factory :updated_donation do\n\
\      amount 400\n\
\    end\n\
\\n\
\    factory :parent do\n\
\      created_at DateTime.now\n\
\      amount 500\n\
\      id 1\n\
\      parent_id nil\n\
\      factory :old_donation do\n\
\        created_at 4.days.ago\n\
\      end\n\
\      factory :unchallenged_donation do\n\
\        is_challenged false\n\
\      end\n\
\      factory :paid_donation do\n\
\        is_paid true\n\
\      end\n\
\    end\n\
\\n\
\    factory :child do\n\
\      amount 100\n\
\      id 2\n\
\      parent_id 1\n\
\      factory :updated_child do\n\
\        amount 500\n\
\      end\n\
\    end\n\
\\n\
\    factory :grandchild do\n\
\      amount 700\n\
\      id 3\n\
\      parent_id 2\n\
\      factory :updated_grandchild do\n\
\        amount 600\n\
\      end\n\
\    end\n\
\\n\
\    factory :second_grandchild do\n\
\      amount 400\n\
\      id 4\n\
\      parent_id 2\n\
\      factory :updated_second_grandchild do\n\
\        amount 500\n\
\      end\n\
\    end\n\
\\n\
\    factory :second_child do\n\
\      amount 100\n\
\      id 5\n\
\      parent_id 1\n\
\    end\n\
\\n\
\    factory :third_child do\n\
\      amount 100\n\
\      id 6\n\
\      parent_id 1\n\
\    end\n\
\\n\
\  end\n\
\end"

test="\
\require 'rails_helper'\n\
\\n\
\RSpec.configure do |c|\n\
\  c.include StripeHelpers\n\
\end\n\
\RSpec.describe StripeClient do\n\
\\n\
\  before(:each) do\n\
\    @stripe_client = StripeClient.new\n\
\  end\n\
\\n\
\  describe '#create_stripe_user' do\n\
\    it 'successfully saves a stripe user' do\n\
\      result = VCR.use_cassette('service_create_stripe_user') do\n\
\        @stripe_client.create_stripe_user(valid_stripe_params)\n\
\      end\n\
\      expect(result).not_to be_falsey\n\
\    end\n\
\\n\
\    it 'fails to create a stripe user' do\n\
\      VCR.use_cassette('failed_stripe_user') do\n\
\        expect{@stripe_client.create_stripe_user(email: 'test@test.com', token: '12345')}.to raise_error(Stripe::InvalidRequestError)\n\
\      end\n\
\    end\n\
\  end\n\
\\n\
\  describe '#retrieve_stripe_user' do\n\
\    context 'has a valid token' do\n\
\      it 'successfully retrieves the stripe user' do\n\
\        user = create(:stripe_user)\n\
\        VCR.use_cassette('retrieve_user') do\n\
\          expect{@stripe_client.retrieve_stripe_user user}.not_to raise_error\n\
\        end\n\
\      end\n\
\    end\n\
\    context 'has an invalid user' do\n\
\      it 'raises an error' do\n\
\        user = create(:user)\n\
\        VCR.use_cassette('retrieve_invalid_user') do\n\
\          expect{@stripe_client.retrieve_stripe_user user}.to raise_error(Stripe::InvalidRequestError)\n\
\        end\n\
\      end\n\
\    end\n\
\  end\n\
\\n\
\  describe '#create_credit_card' do\n\
\    it 'successfully creates credit card' do\n\
\\n\
\      user = create(:stripe_user)\n\
\      customer = fetch_stripe_user(user)\n\
\      allow(@stripe_client)\n\
\        .to receive(:retrieve_stripe_user)\n\
\        .and_return(customer)\n\
\\n\
\      VCR.use_cassette('create_credit_card') do\n\
\        expect{@stripe_client.create_credit_card create_token, user}.not_to raise_error\n\
\      end\n\
\    end\n\
\\n\
\    it 'fails to create credit card because token is invalid' do\n\
\      user = create(:stripe_user)\n\
\\n\
\      customer = fetch_stripe_user(user)\n\
\      allow(@stripe_client)\n\
\        .to receive(:retrieve_stripe_user)\n\
\        .and_return(customer)\n\
\\n\
\      VCR.use_cassette('create_credit_card_invalid_token') do\n\
\        expect{@stripe_client.create_credit_card '12345', user}.to raise_error(Stripe::InvalidRequestError)\n\
\      end\n\
\    end\n\
\  end\n\
\\n\
\  describe '#create_charge' do\n\
\    context 'has a valid stripe id' do\n\
\      it 'creates a charge' do\n\
\        donation = create(:stripe_donation)\n\
\        organization = create(:organization)\n\
\        customer = create_stripe_user(organization)\n\
\        stripe_client = StripeClient.new(organization)\n\
\\n\
\        VCR.use_cassette('create_charge') do\n\
\          expect{stripe_client.create_charge donation, customer}\n\
\            .not_to raise_error\n\
\        end\n\
\        expect(donation.reload.stripe_id).to be\n\
\      end\n\
\    end\n\
\  end\n\
\\n\
\  describe '#create_subscription' do\n\
\    context 'has a valid stripe id' do\n\
\      it 'creates a subscription' do\n\
\        donation = create(:stripe_donation)\n\
\        organization = create(:organization)\n\
\        stripe_client = StripeClient.new(organization)\n\
\        customer = create_stripe_user(organization)\n\
\        VCR.use_cassette('create_subscription') do\n\
\          expect{stripe_client.create_subscription donation, customer}\n\
\            .not_to raise_error\n\
\        end\n\
\        expect(donation.reload.stripe_id).to be\n\
\      end\n\
\    end\n\
\  end\n\
\\n\
\  describe '#create_stripe_token' do\n\
\    it 'creates a token' do\n\
\      user = create(:stripe_user)\n\
\      organization = create(:organization)\n\
\      stripe_client = StripeClient.new(organization)\n\
\      VCR.use_cassette('create_stripe_token_for_organizations_user') do\n\
\        expect(stripe_client.create_stripe_token user.stripe_id).to be\n\
\      end\n\
\    end\n\
\  end\n\
\end"

joinModel="\
\class OrganizationsUser < ActiveRecord::Base\n\
\  belongs_to :organization\n\
\  belongs_to :user\n\
\\n\
\  def self.get_stripe_user(organization, user)\n\
\    organization_user = find_or_create(organization.id, user.id)\n\
\    organization_user.get_stripe_user\n\
\  end\n\
\\n\
\  def self.find_or_create(organization_id, user_id)\n\
\    query_hash = {organization_id: organization_id, user_id: user_id}\n\
\    organizations_user = self.where(query_hash).first\n\
\    organizations_user ||= self.create(query_hash)\n\
\  end\n\
\\n\
\  def get_stripe_user\n\
\    if self.stripe_id.present?\n\
\      stripe_client = StripeClient.new self.organization\n\
\      stripe_client.retrieve_stripe_user self.user\n\
\    else\n\
\      create_stripe_user\n\
\    end\n\
\  end\n\
\\n\
\  def create_stripe_user\n\
\    stripe_client = StripeClient.new(self.organization)\n\
\    token_id = stripe_client.create_stripe_token(self.user.stripe_id)\n\
\    customer = stripe_client.create_stripe_user(token: token_id, email: self.user.email)\n\
\    save_stripe_user(customer)\n\
\  end\n\
\\n\
\  private\n\
\\n\
\  def save_stripe_user(customer)\n\
\    self.stripe_id = customer.id\n\
\    self.save\n\
\    customer\n\
\  end\n\
\\n\
\  def stripe_user_params(token_id)\n\
\    [{\n\
\        source: token_id,\n\
\        email: self.user.email\n\
\    }, self.organization.stripe_access_token]\n\
\  end\n\
\\n\
\end"

orgModel ="\
\require 'rest_client'\n\
\# Stores information related to a specific nonprofit\n\
\# This includes authorization key to organization's stripe account\n\
\class Organization < ActiveRecord::Base\n\
\  has_many :donations\n\
\  has_many :organizations_user\n\
\  has_many :users, through: :organizations_user\n\
\  attr_reader :donation_count\n\
\  attr_reader :donation_amount\n\
\\n\
\  def donation_count\n\
\    User.includes(:donations)\n\
\      .where('donations.organization_id = ? AND donations.is_paid = true', self.id)\n\
\      .references(:donations)\n\
\      .count\n\
\  end\n\
\\n\
\  def donation_amount\n\
\    total = 0\n\
\    Donation.where(organization_id: self.id, is_paid: true)\n\
\      .each{|donation| total += donation.yearly_amount}\n\
\    total\n\
\  end\n\
\\n\
\  def get_stripe_user(user)\n\
\    organizations_user = OrganizationsUser.find_or_create self.id, user.id\n\
\    if organizations_user.stripe_id.present?\n\
\      Stripe.api_key = self.stripe_access_token\n\
\      Stripe::Customer.retrieve organizations_user.stripe_id\n\
\    else\n\
\      customer = create_stripe_user user.stripe_id, user.email\n\
\      organizations_user.stripe_id = customer.id\n\
\      organizations_user.save\n\
\      customer\n\
\    end\n\
\  end\n\
\\n\
\  def create_stripe_user(customer_id, email)\n\
\    Stripe.api_key = self.stripe_access_token\n\
\    token_id = create_stripe_token(customer_id)\n\
\    customer = Stripe::Customer.create({\n\
\      source: token_id,\n\
\      email: email\n\
\    }, self.stripe_access_token)\n\
\    customer\n\
\  end\n\
\\n\
\  def create_stripe_token(customer_id)\n\
\    Stripe.api_key = Rails.application.secrets.stripe_secret_key\n\
\    token = Stripe::Token.create(\n\
\      { customer: customer_id},\n\
\      self.stripe_access_token\n\
\    )\n\
\    token.id\n\
\  end\n\
\\n\
\  def set_access_token(code)\n\
\      response = RestClient.post('https://connect.stripe.com/oauth/token', oauth_params(code))\n\
\      handle_response response\n\
\  end\n\
\\n\
\  private\n\
\  def oauth_params(code)\n\
\    {\n\
\      client_secret: Rails.application.secrets.stripe_secret_key,\n\
\      code: code,\n\
\      grant_type: 'authorization_code',\n\
\      accept: :json,\n\
\      content_type: :json\n\
\    }\n\
\  end\n\
\\n\
\  def handle_response(response)\n\
\    response = JSON.parse(response)\n\
\    self.stripe_access_token = response['access_token']\n\
\    self.stripe_id = response['stripe_user_id']\n\
\    self.save\n\
\  end\n\
\end"

strategy ="\
\module Payments\n\
\  class OneTimePayment\n\
\    def initialize(donation)\n\
\      @donation = donation\n\
\      @stripe_client = StripeClient.new(donation.organization)\n\
\    end\n\
\\n\
\    def pay\n\
\      unless @donation.is_paid\n\
\        customer = OrganizationsUser.get_stripe_user(@donation.organization, @donation.user)\n\
\        @stripe_client.create_charge(@donation, customer)\n\
\      end\n\
\    end\n\
\  end\n\
\end"

paymentService="\
\class PaymentService\n\
\  def initialize(strategy_array)\n\
\    @strategy_array = strategy_array\n\
\  end\n\
\\n\
\  def pay\n\
\    @strategy_array.each{|strategy| strategy.pay}\n\
\  end\n\
\end"

factory ="\
\module Payments\n\
\  class PaymentFactory\n\
\    def initialize(donation)\n\
\      @child = donation\n\
\      @parent = donation.parent\n\
\      @strategy_array = []\n\
\      build_strategies\n\
\      @strategy_array\n\
\    end\n\
\\n\
\    def self.new(*args, &block)\n\
\      obj = self.allocate\n\
\      array = obj.send :initialize, *args, &block\n\
\      PaymentService.new array\n\
\    end\n\
\\n\
\    private\n\
\    def build_strategies\n\
\      child_build\n\
\      parent_build\n\
\    end\n\
\\n\
\    def child_build\n\
\      unless @child.is_challenged\n\
\        add_strategy @child\n\
\      end\n\
\    end\n\
\\n\
\    def parent_build\n\
\      policy = CompletedChallengePolicy.new @parent\n\
\      if @parent.present? && policy.challenge_completed?\n\
\        add_strategy @parent\n\
\      end\n\
\    end\n\
\\n\
\    def add_strategy(donation)\n\
\      strategy = choose_strategy donation\n\
\      if strategy.present?\n\
\        @strategy_array.push strategy.new(donation)\n\
\      end\n\
\    end\n\
\\n\
\    def choose_strategy(donation)\n\
\      if donation.is_subscription\n\
\        Payments::SubscriptionPayment\n\
\      else\n\
\        Payments::OneTimePayment\n\
\      end\n\
\    end\n\
\\n\
\  end\n\
\end"

policy = "\
\class CompletedChallengePolicy\n\
\  def initialize(donation)\n\
\    @donation = donation\n\
\  end\n\
\\n\
\  def challenge_completed?\n\
\    @donation.children.count == 3 && self.can_still_complete?\n\
\  end\n\
\\n\
\  def can_still_complete?\n\
\    @donation.is_challenged && @donation.created_at > 3.days.ago\n\
\  end\n\
\end"

stripe = "\
\class StripeClient\n\
\  def initialize(organization=nil)\n\
\    if organization.present?\n\
\      @organization = organization\n\
\      @secret_key = organization.stripe_access_token\n\
\    else\n\
\      @secret_key = Rails.application.secrets.stripe_secret_key\n\
\    end\n\
\  end\n\
\\n\
\  def create_subscription(donation, customer)\n\
\    subscription = customer.subscriptions\n\
\      .create(subscription_params(donation), stripe_account: @organization.stripe_id)\n\
\    donation.update_attribute('stripe_id', subscription.id)\n\
\  end\n\
\\n\
\  def create_charge(donation, customer)\n\
\    charge = Stripe::Charge\n\
\      .create(charge_params(customer, donation), stripe_account: @organization.stripe_id)\n\
\    donation.update_attribute('stripe_id', charge.id)\n\
\  end\n\
\\n\
\  def create_stripe_token(customer_id)\n\
\    Stripe.api_key = Rails.application.secrets.stripe_secret_key\n\
\    token = Stripe::Token.create(\n\
\      { customer: customer_id},\n\
\      @secret_key\n\
\    )\n\
\    token.id\n\
\  end\n\
\\n\
\  def create_stripe_user(params)\n\
\    Stripe.api_key = @secret_key\n\
\    customer = Stripe::Customer.create(\n\
\      {\n\
\        source: params[:token],\n\
\        email: params[:email]\n\
\      }\n\
\    )\n\
\    customer\n\
\  end\n\
\\n\
\  def retrieve_stripe_user(user)\n\
\    Stripe.api_key = @secret_key\n\
\    Stripe::Customer.retrieve(user.stripe_id)\n\
\  end\n\
\\n\
\  def create_credit_card(token, user)\n\
\    Stripe.api_key = @secret_key\n\
\    customer = self.retrieve_stripe_user user\n\
\    customer.sources.create(:source => token)\n\
\  end\n\
\\n\
\  private\n\
\  def subscription_params(donation)\n\
\    {\n\
\      application_fee_percent: PERCENTAGE_FEE,\n\
\      plan: 'pledge',\n\
\      quantity: donation.amount,\n\
\    }\n\
\  end\n\
\\n\
\  def charge_params(customer, donation)\n\
\    {\n\
\      amount: donation.amount,\n\
\      application_fee: (donation.amount * (PERCENTAGE_FEE / 100.0)).round,\n\
\      currency: 'usd',\n\
\      customer: customer.id\n\
\    }\n\
\  end\n\
\end"

newDonationController ="\
\module Api\n\
\  module V1\n\
\    class DonationsController < ApplicationController\n\
\      before_action :authenticate_user!, except: [:show]\n\
\      def create\n\
\        @donation = Donation.new donation_params\n\
\        donation_decorator = DonationDecorator.new(@donation, card_params, current_user, params[:subscribe], params[:referral_code])\n\
\        respond_to_create donation_decorator\n\
\      end\n\
\\n\
\      def show\n\
\        @donation = Donation.find params[:id]\n\
\        render json: {donation: @donation, name: @donation.user.name}, status: :ok\n\
\      end\n\
\\n\
\      def update\n\
\        @donation = Donation.find params[:id]\n\
\        return render(json: {error:\"Unauthorized\" }, status: :unauthorized) unless @donation.is_owner? current_user.id\n\
\        if @donation.update donation_params\n\
\          render json: {donation: @donation}, status: :ok\n\
\        else\n\
\          render json: @donation.errors, status: :unprocessable_entity\n\
\        end\n\
\      end\n\
\\n\
\      private\n\
\\n\
\      def respond_to_create(donation_decorator)\n\
\        begin\n\
\          donation_decorator.save!\n\
\        rescue Stripe::StripeError => error\n\
\          return render json: {error: error.message}, status: :unprocessable_entity\n\
\        end\n\
\        render json: {donation: donation_decorator.donation}, status: :created\n\
\      end\n\
\\n\
\      def donation_params\n\
\        params.require(:donation).permit(:parent_id, :amount, :organization_id, :is_default, :is_subscription, :is_challenged)\n\
\      end\n\
\\n\
\      def card_params\n\
\        params.require(:card).require(:email)\n\
\        params.require(:card).require(:token)\n\
\        params.require(:card).permit(:email, :token)\n\
\      end\n\
\\n\
\    end\n\
\  end\n\
\end"

oldDonationController = "\
\module Api\n\
\  module V1\n\
\    class DonationsController < ApplicationController\n\
\      before_action :authenticate_user!, except: [:show]\n\
\      def create\n\
\        params_contain_card = card_params[:email] && card_params[:token]\n\
\        if params_contain_card\n\
\          response = current_user.save_stripe_user(card_params)\n\
\          @donation = Donation.new donation_params\n\
\          @donation.user_id = current_user.id\n\
\          if response[:status] == :success\n\
\            #begin\n\
\              if @donation.save\n\
\                if params[:subscribe]\n\
\                  current_user.mailing_subscribe('c8e3eb0f3a')\n\
\                end\n\
\                render json: {donation: @donation}, status: :created\n\
\              else\n\
\                render json: @donation.errors, status: :unprocessable_entity\n\
\              end\n\
\            #rescue => error\n\
\              #render json: {error: error}, status: :unprocessable_entity\n\
\            #end\n\
\          else\n\
\            render json: {error: response[:error].message}, status: :unprocessable_entity\n\
\          end\n\
\        end\n\
\      end\n\
\\n\
\      def show\n\
\        @donation = Donation.find params[:id]\n\
\        render json: {donation: @donation, name: @donation.user.name}, status: :ok\n\
\      end\n\
\\n\
\      def update\n\
\        @donation = Donation.find params[:id]\n\
\        return render(json: {error:\"Unauthorized\" }, status: :unauthorized) unless @donation.is_owner? current_user.id\n\
\        if @donation.update donation_params\n\
\          render json: {donation: @donation}, status: :ok\n\
\        else\n\
\          render json: @donation.errors, status: :unprocessable_entity\n\
\        end\n\
\      end\n\
\\n\
\      private\n\
\\n\
\      def donation_params\n\
\        params.require(:donation).permit(:parent_id, :amount, :organization_id, :is_default, :is_subscription, :is_challenged)\n\
\      end\n\
\\n\
\      def card_params\n\
\        params.require(:card).require(:email)\n\
\        params.require(:card).require(:token)\n\
\        params.require(:card).permit(:email, :token)\n\
\      end\n\
\\n\
\    end\n\
\  end\n\
\end"

decorator =  "\
\class DonationDecorator\n\
\  def initialize(donation, card_params, user, subscribe_to_mail, referrer = nil)\n\
\    @donation = donation\n\
\    @donation.user = user\n\
\    @donation.parent_id = ReferralCodeService.find_id_by_code(referrer)\n\
\    @subscribe_to_mail = subscribe_to_mail\n\
\    @card_params = card_params\n\
\  end\n\
\\n\
\  def donation\n\
\    @donation\n\
\  end\n\
\\n\
\  def save!\n\
\    if contains_card\n\
\      do_transaction\n\
\    else\n\
\      raise 'No card information is passed'\n\
\    end\n\
\  end\n\
\\n\
\  private\n\
\  def do_transaction\n\
\    @donation.transaction do\n\
\      donation_flow\n\
\    end\n\
\  end\n\
\\n\
\  def donation_flow\n\
\    StripeUserService.new(@donation.user).save_stripe_user(@card_params)\n\
\    @donation.save!\n\
\    Payments::PaymentFactory.new(donation).pay\n\
\    NotificationService.new(@donation).send_mail\n\
\    subscribe_to_mail\n\
\  end\n\
\\n\
\  def subscribe_to_mail\n\
\    if @subscribe_to_mail\n\
\      MailingListService.new(@donation.user).mailing_subscribe('c8e3eb0f3a')\n\
\    end\n\
\  end\n\
\\n\
\  def contains_card\n\
\    @card_params[:email].present? && @card_params[:token].present?\n\
\  end\n\
\end"

concern =  "\
\module Pledgeable\n\
\  extend ActiveSupport::Concern\n\
\  included do\n\
\    after_create :after_create\n\
\    before_destroy :delete_subscription\n\
\  end\n\
\\n\
\  def yearly_amount\n\
\    if self.is_subscription && self.is_cancelled && self.cancelled_time.present?\n\
\      months = ((self.cancelled_time.to_f - self.created_at.to_f) / (3600 * 24 * 30)).ceil\n\
\      self.amount * months\n\
\    else\n\
\      yearly = self.amount\n\
\      if self.is_subscription\n\
\        yearly *= 12\n\
\      end\n\
\      yearly\n\
\    end\n\
\  end\n\
\\n\
\  def after_create\n\
\    parent = self.parent\n\
\    unless self.is_challenged\n\
\      response = self.purchase\n\
\      if response[:status] == :failed\n\
\        raise Exception.new(response[:error])\n\
\      end\n\
\      response\n\
\    end\n\
\    if parent.present? && parent.challenge_completed?\n\
\      parent.purchase\n\
\    end\n\
\    self.send_mail\n\
\  end\n\
\\n\
\  def send_mail\n\
\    parent = self.parent\n\
\    if self.user.is_subscribed\n\
\      if self.is_challenged\n\
\        NotificationMailer.pledged(self.user, self).deliver_now\n\
\      else\n\
\        NotificationMailer.donated(self.user, self).deliver_now\n\
\      end\n\
\      if parent.present?\n\
\        grandparent = parent.parent\n\
\        if grandparent.present? && grandparent.one_grandchild\n\
\          NotificationMailer.first_grandchild(grandparent.user, grandparent).deliver_now\n\
\        end\n\
\\n\
\        if parent.can_still_complete?\n\
\          if parent.children.count == 1\n\
\            NotificationMailer.first_friend(parent.user, parent, self.user).deliver_now\n\
\          elsif parent.children.count == 2\n\
\            NotificationMailer.second_friend(parent.user, parent, self.user).deliver_now\n\
\          end\n\
\        end\n\
\\n\
\        if parent.challenge_completed?\n\
\          NotificationMailer.finish_challenge(parent.user, self.user).deliver_now\n\
\        end\n\
\      end\n\
\    end\n\
\  end\n\
\\n\
\  def can_still_complete?\n\
\    self.is_challenged && self.created_at > 3.days.ago\n\
\  end\n\
\\n\
\  def challenge_completed?\n\
\    self.children.count == 3 && self.can_still_complete?\n\
\  end\n\
\\n\
\  def create_subscription\n\
\    begin\n\
\      Stripe.api_key = Rails.application.secrets.stripe_secret_key\n\
\      customer = self.organization.get_stripe_user(user)\n\
\      subscription = customer.subscriptions.create({\n\
\        application_fee_percent: PERCENTAGE_FEE,\n\
\        plan: 'pledge',\n\
\        quantity: self.amount,\n\
\      }, stripe_account: self.organization.stripe_id)\n\
\      self.update_attribute('stripe_id', subscription.id)\n\
\    rescue => error\n\
\      return {status: :failed, error: error}\n\
\    end\n\
\    {status: :success}\n\
\  end\n\
\\n\
\  def create_charge\n\
\    begin\n\
\      Stripe.api_key = Rails.application.secrets.stripe_secret_key\n\
\      customer = self.organization.get_stripe_user(user)\n\
\      charge = Stripe::Charge.create({\n\
\        amount: amount,\n\
\        application_fee: (amount * (PERCENTAGE_FEE / 100.0)).round,\n\
\        currency: 'usd',\n\
\        customer: customer.id\n\
\      }, stripe_account: self.organization.stripe_id)\n\
\      self.update_attribute('stripe_id', charge.id)\n\
\    rescue => error\n\
\      return {status: :failed, error: error}\n\
\    end\n\
\    {status: :success}\n\
\  end\n\
\\n\
\  def purchase\n\
\    unless self.is_paid\n\
\      response = self.is_subscription ? self.create_subscription : self.create_charge\n\
\      self.update(is_paid: true) if response[:status] == :success\n\
\      return response\n\
\    end\n\
\    return {status: :failed, error: 'order has already been paid'}\n\
\  end\n\
\\n\
\  def user_cycles?\n\
\    if self.parent.nil?\n\
\      false\n\
\    else\n\
\      !(self.parent.find_cycle self.user).nil?\n\
\    end\n\
\  end\n\
\\n\
\  def find_cycle user\n\
\    if self.user == user\n\
\      self\n\
\    elsif self.parent.nil?\n\
\      nil\n\
\    else\n\
\      self.parent.find_cycle user\n\
\    end\n\
\  end\n\
\\n\
\  def subscription_length(starting_timestamp)\n\
\    if self.is_subscription\n\
\      today = Time.now\n\
\      (today.year * 12 + today.month) - (starting_timestamp.year * 12 + starting_timestamp.month)\n\
\    else\n\
\      0\n\
\    end\n\
\  end\n\
\\n\
\  def delete_subscription\n\
\    if self.is_subscription && self.is_paid\n\
\      customer = self.organization.get_stripe_user self.user\n\
\      subscriptions = customer.subscriptions\n\
\      unless subscriptions.data.empty?\n\
\        result = subscriptions.retrieve(self.stripe_id)\n\
\        self.update_attribute('is_cancelled', true)\n\
\        self.update_attribute('cancelled_time', DateTime.now)\n\
\        result.delete\n\
\      end\n\
\    end\n\
\  end\n\
\end"

presentation ∷ Array SlideData
presentation =
  [
    SlideData
      { fileName: ""
      , filePath: ""
      , lineNumber: 1
      , language: "ruby"
      , beforeOrAfter: Nothing
      , annotation: "Hi! I want to walk you through a project of mine, the pieces that were broken windows, and how I fixed them. This project is the API from a startup I have been working on. This was one of the first projects I did using test driven development. I went back and refactored large pieces of it and I want to walk you through that process."
      , content: "\n"

      }
  , SlideData
      { fileName: "pledgeable.rb"
      , filePath: "app/models/concerns"
      , lineNumber: 1
      , language: "ruby"
      , beforeOrAfter: Before
      , annotation: "There were several code smells and broken windows I wanted to address. The brunt of the logic involves creating donations and pledging. My original idea is, as you can see, not the best. I knew I didn't want fat models so I put all of my logic in this concern. It got big and unwieldy quickly. Not only was this a problem because a single file was doing a lot of unrelated things, but I also now knew to use composition over inheritance. So I wanted to refactor this into a bunch of small single responsibility plain ruby objects."
      , content: concern
      }

  , SlideData
      { fileName: "pledgeable.rb"
      , filePath: "app/models/concerns"
      , lineNumber: 21
      , language: "ruby"
      , beforeOrAfter: Before
      , annotation: "As you can see there is a semi complicated after_create hook that executes business logic. I learned through Thoughtbot Upcase videos and \"Ruby Science\" that this may not be the best idea. I decided to refactor this to move away from these smells."
      , content: concern
      }

  , SlideData
      { fileName: "donation_decorator.rb"
      , filePath: "app/decorators"
      , lineNumber: 1
      , language: "ruby"
      , beforeOrAfter: After
      , annotation: "To remove business logic from this after create hook I created this decorator that allows me to append actions after model creation. This make my program easier to test and makes it read much more clearly."
      , content: decorator
      }

  , SlideData
      { fileName: "donation_decorator.rb"
      , filePath: "app/decorators"
      , lineNumber: 29
      , language: "ruby"
      , beforeOrAfter: After
      , annotation: "Here is the main flow of the decorator. In retrospect I may have gone to far in abstracting out functions. However this still reads very clearly. Overall I am quite pleased with readability gained by using a decorator this way."
      , content: decorator
      }

   , SlideData
      { fileName: "donations_controller.rb"
      , filePath: "app/controllers/api/v1"
      , lineNumber: 5
      , language: "ruby"
      , beforeOrAfter: Before
      , annotation: "Also my controller was quite complicated before I created this decorator. I not only moved after_create logic into this class but much of this controller logic."
      , content: oldDonationController
      }

   , SlideData
      { fileName: "donations_controller.rb"
      , filePath: "app/controllers/api/v1"
      , lineNumber: 5
      , language: "ruby"
      , beforeOrAfter: After
      , annotation: "The create method on the controller is now much neater and all the messy logic is handled in the decorator"
      , content: newDonationController
      }

   , SlideData
      { fileName: "pledgeable.rb"
      , filePath: "app/models/concerns"
      , lineNumber: 73
      , language: "ruby"
      , beforeOrAfter: Before
      , annotation: "The donations are handled in Stripe and the logic originally also lived in the same concern. Once again this is quite messy and doesn't really belong here."
      , content: concern
      }

   , SlideData
      { fileName: "stripe_client.rb"
      , filePath: "app/models"
      , lineNumber: 1
      , language: "ruby"
      , beforeOrAfter: After
      , annotation: "I abstracted the Stripe logic out to it's own separate class. This way all the Stripe logic is in one file and thus this is the only place we make external calls to Stripe. One concern with this approach is feature envy. Several of the methods in here use their parameters more than the class itself. However I decided the simplicity was worth code smell."
      , content: stripe
      }

   , SlideData
      { fileName: "pledgeable.rb"
      , filePath: "app/models/concerns"
      , lineNumber: 69
      , language: "ruby"
      , beforeOrAfter: Before
      , annotation: "We had a gamified donation system and with that a challenge that must be passed before you can donate. It's in the same concern as everything else was! It's very clear at this point that this concern is nowhere near following the single responsibility principle."
      , content: concern
      }

   , SlideData
      { fileName: "completed_challenge_policy"
      , filePath: "app/policies"
      , lineNumber: 1
      , language: "ruby"
      , beforeOrAfter: After
      , annotation: "This seemed like the perfect use case for a policy object. Now it is a class with a single job!"
      , content: policy
      }

   , SlideData
      { fileName: "pledgeable.rb"
      , filePath: "app/models/concerns"
      , lineNumber: 21
      , language: "ruby"
      , beforeOrAfter: Before
      , annotation: "The policy is just one piece of determining if a charge should be made. When a donation is created there are several things that can happen. In this after create either the created donation, the referrer donation, or both could be charged."
      , content: concern
      }

   , SlideData
      { fileName: "pledgeable.rb"
      , filePath: "app/models/concerns"
      , lineNumber: 106
      , language: "ruby"
      , beforeOrAfter: Before
      , annotation: "We also had to see if the donation was a one time payment or a subscription."
      , content: concern
      }

    , SlideData
      { fileName: "payment_factory.rb"
      , filePath: "app/models/payments"
      , lineNumber: 1
      , language: "ruby"
      , beforeOrAfter: After
      , annotation: "I decided to move this logic into a factory that would build a payment service."
      , content: factory
      }

    , SlideData
      { fileName: "payment_factory.rb"
      , filePath: "app/models/payments"
      , lineNumber: 23
      , language: "ruby"
      , beforeOrAfter: After
      , annotation: "This factory knows which donations to charge and checks on both the parent and the child."
      , content: factory
      }

    , SlideData
      { fileName: "payment_factory.rb"
      , filePath: "app/models/payments"
      , lineNumber: 43
      , language: "ruby"
      , beforeOrAfter: After
      , annotation: "It also determines if it should be a OneTimePayment or SubscriptionPayment strategy."
      , content: factory
      }

    , SlideData
      { fileName: "one_time_payment.rb"
      , filePath: "app/models/payments"
      , lineNumber: 1
      , language: "ruby"
      , beforeOrAfter: After
      , annotation: "This is an example of one of the strategies built by the payment_factory. All payment strategies implement a pay method that charge Stripe in various ways."
      , content: strategy
      }

    , SlideData
      { fileName: "payment_service.rb"
      , filePath: "app/services"
      , lineNumber: 1
      , language: "ruby"
      , beforeOrAfter: After
      , annotation: "This is the service built by the factory we just saw. It is very simple and just calls pay on the strategies created by the factory."
      , content: paymentService
      }

    , SlideData
      { fileName: "organization.rb"
      , filePath: "app/models"
      , lineNumber: 25
      , language: "ruby"
      , beforeOrAfter: Before
      , annotation: "There is a lot of logic in here dealing with finding the correct user for a purchase. This is not great because it is a case of feature envy. Several of these methods are more interested in their parameters and OrganizationUser than they are in the class they are in. I did two different things to fix this problem."
      , content: orgModel
      }

    , SlideData
      { fileName: "organizations_user.rb"
      , filePath: "app/models"
      , lineNumber: 16
      , language: "ruby"
      , beforeOrAfter: After
      , annotation: "First I moved several of these methods to OrganizationUser to fix the feature envy. I then refactored them to just call the previously mentioned StripeClient. This made the logic much simpler and helped me not repeat myself."
      , content: joinModel
      }

    , SlideData
      { fileName: "stripe_client_spec.rb"
      , filePath: "spec/models"
      , lineNumber: 31
      , language: "ruby"
      , beforeOrAfter: After
      , annotation: "I also wanted an easy way to speed up tests while still keeping the original behavior. Especially with code that was calling out to Stripe. I decided to use VCR to record the result of the call the first time and after that just use those recordings. Here is an example of me using VCR."
      , content: test
      }

    , SlideData
      { fileName: "donation.rb"
      , filePath: "spec/factories"
      , lineNumber: 1
      , language: "ruby"
      , beforeOrAfter: Before
      , annotation: "I also had a lot of donation factories originally. I decided this constituted a mystery guest."
      , content: factories
      }

    , SlideData
      { fileName: "donation.rb"
      , filePath: "spec/factories"
      , lineNumber: 1
      , language: "ruby"
      , beforeOrAfter: After
      , annotation: "So I simplified it to this and kept just the basic ones. I then set the fields I needed in the respective tests. This made my tests easier to follow but made some of them longer. I am willing to accept this tradeoff for now."
      , content: newFactory
      }

    , SlideData
      { fileName: ""
      , filePath: ""
      , lineNumber: 1
      , language: "ruby"
      , beforeOrAfter: Nothing
      , annotation: "I hope you have enjoyed this walkthrough of a refactoring I have done. Feel free to checkout my github (https://github.com/frasermince) and look at the other projects I have done. You might be interested in some of the work I've done in Haskell (https://github.com/frasermince/mal-personal, https://github.com/frasermince/PortfolioAPI) or Javascript (https://github.com/frasermince/CodePortfolio, https://github.com/frasermince/MultiplyMe)"
      , content: "\n"
      }

    , SlideData
      { fileName: ""
      , filePath: ""
      , lineNumber: 1
      , language: "ruby"
      , beforeOrAfter: Nothing
      , annotation: "And finally I hope you have enjoyed trying out my newest product. This code portfolio software. If you're interested in learning more about it or making presentations like this yourself shoot me an email at frasermince@gmail.com."
      , content: "\n"
      }
]
