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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.IActief;
import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.rivm.screenit.util.DiffSpecs;
import nl.rivm.screenit.util.SkipFieldForDiff;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;
import nl.topicuszorg.organisatie.model.Adres;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;

@Entity
@Table(schema = "mamma", name = "tehuis")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "mamma.cache")
@Audited
@Getter
@Setter
public class MammaTehuis extends AbstractHibernateObject implements IActief
{
	@Column(nullable = false, length = HibernateMagicNumber.L255)
	private String naam;

	@Column(nullable = false, length = HibernateMagicNumber.L255)
	private String contactpersoon;

	@Column(nullable = false)
	private Boolean actief;

	@ManyToOne(optional = false, fetch = FetchType.LAZY)
	@DiffSpecs(displayProperty = "naam")
	private MammaStandplaats standplaats;

	@OneToMany(mappedBy = "tehuis", fetch = FetchType.LAZY)
	@SkipFieldForDiff
	private List<MammaDossier> dossiers = new ArrayList<>();

	@OneToOne(optional = false, fetch = FetchType.LAZY)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "mamma.cache")
	@SkipFieldForDiff
	private Adres aanschrijfAdres;

	@OneToMany(mappedBy = "tehuis", fetch = FetchType.LAZY)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "organisatie.cache")
	@SkipFieldForDiff
	private List<MammaTehuisAdres> adressen = new ArrayList<>();

	@OneToMany(mappedBy = "tehuis", fetch = FetchType.LAZY)
	@SkipFieldForDiff
	private List<MammaTehuisOpmerking> opmerkingen = new ArrayList<>();

	@Column(nullable = true, length = HibernateMagicNumber.L255)
	private String telefoonnummer;

	@Column(nullable = true)
	@Temporal(TemporalType.DATE)
	private Date uitgenodigd;

	@Override
	public Boolean getActief()
	{
		return actief;
	}
}
