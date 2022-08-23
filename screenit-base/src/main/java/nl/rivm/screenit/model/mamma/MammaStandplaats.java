package nl.rivm.screenit.model.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.List;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.IActief;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.rivm.screenit.util.DiffSpecs;
import nl.rivm.screenit.util.SkipFieldForDiff;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;

@Entity
@Table(schema = "mamma", name = "standplaats", uniqueConstraints = { @UniqueConstraint(columnNames = "locatie"), @UniqueConstraint(columnNames = "tijdelijke_locatie") })
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "mamma.cache")
@Audited
@Getter
@Setter
public class MammaStandplaats extends AbstractHibernateObject implements IActief
{
	@Column(nullable = false, length = HibernateMagicNumber.L255)
	private String naam;
	@OneToMany(mappedBy = "standplaats", fetch = FetchType.LAZY)
	@SkipFieldForDiff
	private List<MammaStandplaatsOpmerking> standplaatsOpmerkingen = new ArrayList<>();
	@Column(nullable = false)
	private Boolean actief;
	@OneToOne(optional = false, fetch = FetchType.LAZY)
	@SkipFieldForDiff
	private MammaStandplaatsLocatie locatie;
	@OneToOne(optional = false, fetch = FetchType.LAZY)
	@SkipFieldForDiff
	private MammaStandplaatsLocatie tijdelijkeLocatie;
	@OneToMany(mappedBy = "standplaats", fetch = FetchType.LAZY)
	@SkipFieldForDiff
	private List<MammaPostcodeReeks> postcodeReeksen = new ArrayList<>();
	@ManyToOne(optional = false, fetch = FetchType.LAZY)
	@DiffSpecs(displayProperty = "naam")
	private ScreeningOrganisatie regio;
	@OneToMany(mappedBy = "standplaats", fetch = FetchType.LAZY)
	private List<MammaBlokkade> blokkades;

	@OneToMany(mappedBy = "standplaats", fetch = FetchType.LAZY)
	private List<MammaStandplaatsRonde> standplaatsRonden = new ArrayList<>();
	@OneToMany(mappedBy = "standplaats", fetch = FetchType.LAZY)
	private List<MammaTehuis> tehuizen = new ArrayList<>();
}
