package nl.rivm.screenit.model.mamma;

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

import java.math.BigDecimal;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;

import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

@Entity
@Table(
	schema = "mamma",
	name = "deelnamekans",
	uniqueConstraints = { @UniqueConstraint(columnNames = "dossier"), })
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "mamma.cache")
public class MammaDeelnamekans extends AbstractHibernateObject
{
	@OneToOne(optional = false, fetch = FetchType.LAZY)
	private MammaDossier dossier;

	@Column(nullable = false, precision = HibernateMagicNumber.P6, scale = HibernateMagicNumber.S5)
	private BigDecimal deelnamekans = BigDecimal.valueOf(0.8);

	public MammaDossier getDossier()
	{
		return dossier;
	}

	public void setDossier(MammaDossier dossier)
	{
		this.dossier = dossier;
	}

	public BigDecimal getDeelnamekans()
	{
		return deelnamekans;
	}

	public void setDeelnamekans(BigDecimal deelnamekans)
	{
		this.deelnamekans = deelnamekans;
	}
}
