
package nl.rivm.screenit.model.colon;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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
import javax.persistence.JoinColumn;
import javax.persistence.JoinTable;
import javax.persistence.ManyToMany;

import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.helper.HibernateMagicNumber;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Cascade;
import org.hibernate.annotations.CascadeType;
import org.hibernate.envers.Audited;

@Entity
@Audited
public class PaLaboratorium extends Instelling
{

	private static final long serialVersionUID = 1L;

	@ManyToMany(fetch = FetchType.LAZY)
	@Cascade(CascadeType.SAVE_UPDATE)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
	@JoinTable(schema = "colon", name = "org_organisatie_coloscopielocaties", joinColumns = { @JoinColumn(name = "org_organisatie") })
	private List<ColoscopieLocatie> coloscopielocaties = new ArrayList<>();

	@Column(nullable = true, length = HibernateMagicNumber.L255)
	private String fqdn;

	public List<ColoscopieLocatie> getColoscopielocaties()
	{
		return coloscopielocaties;
	}

	public void setColoscopielocaties(List<ColoscopieLocatie> coloscopielocaties)
	{
		this.coloscopielocaties = coloscopielocaties;
	}

	public String getFqdn()
	{
		return fqdn;
	}

	public void setFqdn(String fqdn)
	{
		this.fqdn = fqdn;
	}

}
