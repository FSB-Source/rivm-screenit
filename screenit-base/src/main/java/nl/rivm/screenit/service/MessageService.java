package nl.rivm.screenit.service;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
 * %%
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * =========================LICENSE_END==================================
 */

import java.util.List;
import java.util.Optional;

import nl.rivm.screenit.model.messagequeue.Message;
import nl.rivm.screenit.model.messagequeue.MessageType;

import com.fasterxml.jackson.core.JsonProcessingException;

public interface MessageService
{
	void queueMessage(MessageType type, Object content);

	void queueMessage(MessageType type, Object content, String context);

	void dequeueMessage(Message message);

	Optional<Message> getOldestMessage(MessageType type);

	List<Message> fetchMessages(MessageType type, String context, int maxFetchSize);

	<T> T getContent(Message message) throws JsonProcessingException;

	Long fetchQueueSize(MessageType type, String context);
}
