package nl.rivm.screenit.mappers;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.dto.alg.client.contact.MailAttachmentDto;
import nl.rivm.screenit.mappers.config.ScreenitMapperConfig;
import nl.rivm.screenit.model.MailAttachment;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

@Mapper(config = ScreenitMapperConfig.class)
public interface MailAttachmentMapper
{
	@Mapping(target = "id", ignore = true)
	@Mapping(target = "mail", ignore = true)
	MailAttachment mailAttachmentToDto(MailAttachmentDto mailAttachmentDto);

	List<MailAttachment> dtoToAttachmentList(List<MailAttachmentDto> mailAttachments);

}
