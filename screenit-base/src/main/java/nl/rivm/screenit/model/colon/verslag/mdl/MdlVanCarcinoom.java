
package nl.rivm.screenit.model.colon.verslag.mdl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import javax.annotation.Nonnull;
import javax.persistence.AttributeOverride;
import javax.persistence.AttributeOverrides;
import javax.persistence.Column;
import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import nl.rivm.screenit.model.verslag.Quantity;
import nl.rivm.screenit.model.verslag.VraagElement;
import nl.rivm.screenit.model.verslag.VraagElementUnit;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import com.fasterxml.jackson.annotation.JsonIgnore;

@Entity
@Table(schema = "colon")
public class MdlVanCarcinoom
	extends AbstractHibernateObject
{

	private final static long serialVersionUID = 1L;

	@ManyToOne(fetch = FetchType.LAZY)
	@JsonIgnore
	private MdlLaesiecoloscopiecentrum laesiecoloscopiecentrum;

	@Column
	@VraagElement(displayName = "Stenoserend ja/nee", extraTekst = "Stenoserend ja/nee", code = "2.16.840.1.113883.2.4.3.36.77.2.8.145110")
	private Boolean stenoserendJanee;

	@Embedded
	@Nonnull
	@AttributeOverrides({
		@AttributeOverride(name = "value", column = @Column(name = "specifiekeAfstandTumorVanafAnusValue")),
		@AttributeOverride(name = "unit", column = @Column(name = "specifiekeAfstandTumorVanafAnusUnit"))
	})
	@VraagElement(
		displayName = "Specifieke afstand tumor vanaf anus",
		extraTekst = "Specifieke afstand tumor vanaf anus (in cm)",
		code = "2.16.840.1.113883.2.4.3.36.77.2.8.145062",
		isVerplicht = true,
		unit = {
			@VraagElementUnit(unit = "cm", min = "1.0", max = "200.0")
		})
	private Quantity specifiekeAfstandTumorVanafAnus;

	@Column
	@VraagElement(displayName = "Te passeren ja/nee", extraTekst = "Carcinoom is wel/niet te passeren", code = "2.16.840.1.113883.2.4.3.36.77.2.8.145140")
	private Boolean tePasserenJanee;

	public MdlLaesiecoloscopiecentrum getLaesiecoloscopiecentrum()
	{
		return laesiecoloscopiecentrum;
	}

	public void setLaesiecoloscopiecentrum(MdlLaesiecoloscopiecentrum laesiecoloscopiecentrum)
	{
		this.laesiecoloscopiecentrum = laesiecoloscopiecentrum;
	}

	public Boolean getStenoserendJanee()
	{
		return stenoserendJanee;
	}

	public void setStenoserendJanee(Boolean stenoserendJanee)
	{
		this.stenoserendJanee = stenoserendJanee;
	}

	public Quantity getSpecifiekeAfstandTumorVanafAnus()
	{
		return specifiekeAfstandTumorVanafAnus;
	}

	public void setSpecifiekeAfstandTumorVanafAnus(Quantity specifiekeAfstandTumorVanafAnus)
	{
		this.specifiekeAfstandTumorVanafAnus = specifiekeAfstandTumorVanafAnus;
	}

	public Boolean getTePasserenJanee()
	{
		return tePasserenJanee;
	}

	public void setTePasserenJanee(Boolean tePasserenJanee)
	{
		this.tePasserenJanee = tePasserenJanee;
	}

}
