/**
 * Wire
 * Copyright (C) 2017 Wire Swiss GmbH
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package com.waz.zclient.views

import android.content.Context
import android.util.AttributeSet
import android.view.View
import android.widget.LinearLayout
import com.waz.ZLog.ImplicitTag._
import com.waz.api.IConversation
import com.waz.model.{ConvId, UserData}
import com.waz.service.ZMessaging
import com.waz.threading.Threading
import com.waz.utils.events.Signal
import com.waz.zclient.controllers.global.AccentColorController
import com.waz.zclient.ui.text.TypefaceTextView
import com.waz.zclient.ui.utils.ColorUtils
import com.waz.zclient.utils.ViewUtils
import com.waz.zclient.{R, ViewHelper}
import com.waz.zclient.utils.ContextUtils._
import com.waz.utils._

class NewConversationListRow(context: Context, attrs: AttributeSet, style: Int) extends LinearLayout(context, attrs, style) with ViewHelper { self =>
  def this(context: Context, attrs: AttributeSet) = this(context, attrs, 0)
  def this(context: Context) = this(context, null, 0)

  inflate(R.layout.new_conv_list_item)

  val zms = inject[Signal[ZMessaging]]
  val accentColor = inject[AccentColorController].accentColor
  val selfId = zms.map(_.selfUserId)

  private val conversationId = Signal[ConvId]()

  val conversation = for {
    z <- zms
    convId <- conversationId
    conv <- Signal.future(z.convsStorage.get(convId))
  } yield conv

  val conversationName = conversation.map(_.fold("")(_.displayName))

  val conversationInfo = for {
    z <- zms
    Some(conv) <- conversation
    memberIds <- z.membersStorage.activeMembers(conv.id)
    memberSeq <- Signal.future(z.usersStorage.getAll(memberIds))
    self <- selfId
  } yield (conv.convType, memberSeq.flatten.filter(_.id != self))

  val title = ViewUtils.getView(this, R.id.conversation_title).asInstanceOf[TypefaceTextView]
  val subtitle = ViewUtils.getView(this, R.id.conversation_subtitle).asInstanceOf[TypefaceTextView]
  val avatar = ViewUtils.getView(this, R.id.conversation_icon).asInstanceOf[ConversationAvatarView]
  val statusPill = ViewUtils.getView(this, R.id.conversation_status_pill).asInstanceOf[TypefaceTextView]
  val separator = ViewUtils.getView(this, R.id.conversation_separator).asInstanceOf[View]
  var iConversation: IConversation = null

  val unreadCount = for {
    z <- zms
    convId <- conversationId
    unreadCount <- z.messagesStorage.unreadCount(convId)
  } yield unreadCount

  val lastMessageInfo = for {
    z <- zms
    convId <- conversationId
    self <- selfId
    lastMessage <- z.messagesStorage.lastMessage(convId)
    user <- lastMessage.fold2[Signal[Option[UserData]]](Signal.const(Option.empty[UserData]), message => z.usersStorage.optSignal(message.userId))
  } yield (lastMessage, user, lastMessage.exists(_.userId == self))

  val isCurrentConversation = for {
    z <- zms
    convId <- conversationId
    currentConv <- z.convsStats.selectedConversationId
  } yield currentConv.contains(convId)

  conversationName.on(Threading.Ui) { title.setText }

  conversationInfo.on(Threading.Ui) { convInfo  =>
    avatar.setMembers(convInfo._2.flatMap(_.picture), convInfo._1)
  }

  unreadCount.on(Threading.Ui) { count =>
    statusPill.setText(count.toString)
    if (count > 0) {
      statusPill.setVisibility(View.VISIBLE)
    } else {
      statusPill.setVisibility(View.INVISIBLE)
    }
  }

  lastMessageInfo.on(Threading.Ui) {
    case (Some(message), Some(user), false) if message.contentString.nonEmpty =>
      showSubtitle()
      subtitle.setText(s"${user.getDisplayName}: ${message.contentString}")
    case (Some(message), _, _) if message.contentString.nonEmpty =>
      showSubtitle()
      subtitle.setText(s"${message.contentString}")
    case _ =>
      hideSubtitle()
      subtitle.setText("")
  }

  isCurrentConversation.zip(accentColor).on(Threading.Ui){
    case (true, color) =>
      separator.setBackgroundColor(ColorUtils.injectAlpha(0.5f, color.getColor()))
    case (false, _) =>
      separator.setBackgroundColor(getColor(R.color.white_8))
  }

  private def showSubtitle(): Unit = {
    title.setPadding(0, 0, 0, 0)
    subtitle.setVisibility(View.VISIBLE)
  }

  private def hideSubtitle(): Unit = {
    title.setPadding(0, getDimenPx(R.dimen.conversation_list__row__title__top), 0, 0)
    subtitle.setVisibility(View.GONE)
  }

  def needsRedraw: Boolean = false
  def redraw(): Unit = {
    title.setText("")
    conversationName.head.foreach(title.setText)(Threading.Ui)
  }

  def getConversation: IConversation = iConversation
  def setConversation(iConversation: IConversation): Unit = {
    self.iConversation = iConversation
    if (!conversationId.currentValue.contains(ConvId(iConversation.getId))) {
      title.setText(iConversation.getName)
      subtitle.setText("")
      avatar.setMembers(Seq(), iConversation.getType)
    }
    conversationId ! ConvId(iConversation.getId)
  }

  def isArchiveTarget: Boolean = false
}
