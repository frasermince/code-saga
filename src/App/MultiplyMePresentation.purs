module App.MultiplyMePresentation where

import Prelude
import App.Prelude
import App.State (SlideData(..))

presentation ∷ Array SlideData
presentation =
  [
    SlideData
      { fileName: "MultiplyMeApi/app/controllers/api/v1/leader_board_controller.rb"
      , lineNumber: 27
      , annotation: "Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Donec hendrerit tempor tellus. Donec pretium posuere tellus. Proin quam nisl, tincidunt et, mattis eget, convallis nec, purus. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Nulla posuere. Donec vitae dolor. Nullam tristique diam non turpis. Cras placerat accumsan nulla. Nullam rutrum. Nam vestibulum accumsan nisl.orem ipsum dolor sit amet, consectetuer adipiscing elit. Donec hendrerit tempor tellus. Donec pretium posuere tellus. Proin quam nisl, tincidunt et, mattis eget, convallis nec, purus. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Nulla posuere. Donec vitae dolor. Nullam tristique diam non turpis. Cras placerat accumsan nulla. Nullam rutrum. Nam vestibulum accumsan nisl.orem ipsum dolor sit amet, consectetuer adipiscing elit. Donec hendrerit tempor tellus. Donec pretium posuere tellus. Proin quam nisl, tincidunt et, mattis eget, convallis nec, purus. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Nulla posuere. Donec vitae dolor. Nullam tristique diam non turpis. Cras placerat accumsan nulla. Nullam rutrum. Nam vestibulum accumsan nisl.orem ipsum dolor sit amet, consectetuer adipiscing elit. Donec hendrerit tempor tellus. Donec pretium posuere tellus. Proin quam nisl, tincidunt et, mattis eget, convallis nec, purus. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Nulla posuere. Donec vitae dolor. Nullam tristique diam non turpis. Cras placerat accumsan nulla. Nullam rutrum. Nam vestibulum accumsan nisl.orem ipsum dolor sit amet, consectetuer adipiscing elit. Donec hendrerit tempor tellus. Donec pretium posuere tellus. Proin quam nisl, tincidunt et, mattis eget, convallis nec, purus. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Nulla posuere. Donec vitae dolor. Nullam tristique diam non turpis. Cras placerat accumsan nulla. Nullam rutrum. Nam vestibulum accumsan nisl.orem ipsum dolor sit amet, consectetuer adipiscing elit. Donec hendrerit tempor tellus. Donec pretium posuere tellus. Proin quam nisl, tincidunt et, mattis eget, convallis nec, purus. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Nulla posuere. Donec vitae dolor. Nullam tristique diam non turpis. Cras placerat accumsan nulla. Nullam rutrum. Nam vestibulum accumsan nisl."
      , content: "\
\module Api\n\
\  module V1\n\
\    class LeaderBoardController < BaseController\n\
\\n\
\      def index\n\
\        limit = params[:limit].nil? ? 10 : params[:limit]\n\
\        @leaders = get_leaders limit\n\
\        render json: {leaders: @leaders}, status: :ok, methods: :contribution\n\
\      end\n\
\\n\
\      private\n\
\\n\
\      def get_leaders limit\n\
\        User.select(:id, 'users.name as name', 'users.email as email')\n\
\          .to_a\n\
\          .sort_by{|user| user.personal_impact(params[:organization_id]) + user.network_impact(params[:organization_id])}\n\
\          .reverse\n\
\          .first(limit.to_i)\n\
\      end\n\
\    end\n\
\\n\
\  end\n\
\end"

      }
  , SlideData
      { fileName: "MultiplyMeApi/app/services/referral_code_service.rb"
      , lineNumber: 18
      , annotation: "second annotation"
      , content: "\
\class ReferralCodeService\n\
\  def initialize(donation)\n\
\    @donation = donation\n\
\  end\n\
\\n\
\  def generate_code\n\
\    (@donation.user.name.split(\" \")[0] + @donation.id.to_s).downcase\n\
\  end\n\
\\n\
\  def self.find_donation_by_code(code)\n\
\    if code.present?\n\
\      Donation.where(referral_code: code.downcase).first\n\
\    else\n\
\      nil\n\
\    end\n\
\  end\n\
\\n\
\  def self.find_id_by_code(code)\n\
\    donation = find_donation_by_code code\n\
\    if donation.present?\n\
\      donation.id\n\
\    else\n\
\      nil\n\
\    end\n\
\  end\n\
\end"
      }
  , SlideData
      { fileName: "MultiplyMeApi/app/controllers/api/v1/organizations_controller.rb"
      , lineNumber: 1
      , annotation: "third annotation"
      , content: "\
\module Api\n\
\  module V1\n\
\    class OrganizationsController < BaseController\n\
\      def show\n\
\        @organization = Organization.select('name, campaign_end_date, id').find params[:id]\n\
\        render json: {organization: @organization}, status: :ok, methods: [:donation_count, :donation_amount]\n\
\      end\n\
\    end\n\
\  end\n\
\end"
      }
  ]
