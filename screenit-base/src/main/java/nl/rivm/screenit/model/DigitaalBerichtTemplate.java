package nl.rivm.screenit.model;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.time.LocalDateTime;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.Lob;
import javax.persistence.Table;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.enums.DigitaalBerichtTemplateType;
import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.rivm.screenit.util.DiffSpecs;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Type;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Getter
@Setter
@Entity
@Audited
@Table(schema = "algemeen")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
public class DigitaalBerichtTemplate extends AbstractHibernateObject
{
	@Enumerated(EnumType.STRING)
	@Column(nullable = false, unique = true)
	@NotAudited
	private DigitaalBerichtTemplateType type;

	@Column(nullable = false)
	private LocalDateTime gewijzigdOp;

	@DiffSpecs(displayName = "onderwerp")
	@Column(length = HibernateMagicNumber.L255)
	private String subject;

	@Lob
	@Type(type = "org.hibernate.type.TextType")
	@DiffSpecs(displayName = "inhoud")
	@Column(nullable = false)
	private String body;
}
