package nl.rivm.screenit.model.colon;

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

import java.util.Date;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.Index;
import javax.persistence.ManyToOne;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import javax.persistence.UniqueConstraint;

import nl.rivm.screenit.model.InpakbareUitnodiging;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingCategorie;

import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Entity
@Table(
	schema = "colon",
	uniqueConstraints = { @UniqueConstraint(columnNames = "uitnodigingsId") },
	indexes = {
		@Index(name = "idx_colon_uitnodiging_verstuurd", columnList = "verstuurd"),
		@Index(name = "idx_colon_uitnodiging_onderzoeks_variant", columnList = "onderzoeksVariant"),
		@Index(name = "idx_colon_uitnodiging_trackid", columnList = "trackTraceId") })
@Audited
public class ColonUitnodiging extends InpakbareUitnodiging<ColonScreeningRonde>
{

	@Enumerated(EnumType.STRING)
	private ColonUitnodigingCategorie colonUitnodigingCategorie;

	@OneToOne
	private IFOBTTest gekoppeldeTest;

	@OneToOne
	private IFOBTTest gekoppeldeExtraTest;

	@Deprecated
	@OneToOne(cascade = CascadeType.ALL)
	private ScannedAntwoordFormulier antwoordFormulier;

	@ManyToOne(optional = false)
	private ColonScreeningRonde screeningRonde;

	@Enumerated(EnumType.STRING)
	@NotAudited
	@Column(nullable = false)
	private ColonOnderzoeksVariant onderzoeksVariant;

	@Temporal(TemporalType.DATE)
	private Date uitgesteldeUitslagDatum;

	public ColonUitnodigingCategorie getColonUitnodigingCategorie()
	{
		return colonUitnodigingCategorie;
	}

	public void setColonUitnodigingCategorie(ColonUitnodigingCategorie colonUitnodigingCategorie)
	{
		this.colonUitnodigingCategorie = colonUitnodigingCategorie;
	}

	public IFOBTTest getGekoppeldeTest()
	{
		return gekoppeldeTest;
	}

	public void setGekoppeldeTest(IFOBTTest gekoppeldeTest)
	{
		this.gekoppeldeTest = gekoppeldeTest;
	}

	public IFOBTTest getGekoppeldeExtraTest()
	{
		return gekoppeldeExtraTest;
	}

	public void setGekoppeldeExtraTest(IFOBTTest gekoppeldeExtraTest)
	{
		this.gekoppeldeExtraTest = gekoppeldeExtraTest;
	}

	@Deprecated
	public void setAntwoordFormulier(ScannedAntwoordFormulier antwoordFormulier)
	{
		this.antwoordFormulier = antwoordFormulier;
	}

	@Deprecated
	public ScannedAntwoordFormulier getAntwoordFormulier()
	{
		return antwoordFormulier;
	}

	@Override
	public ColonScreeningRonde getScreeningRonde()
	{
		return screeningRonde;
	}

	@Override
	public void setScreeningRonde(ColonScreeningRonde screeningRonde)
	{
		this.screeningRonde = screeningRonde;
	}

	public ColonOnderzoeksVariant getOnderzoeksVariant()
	{
		return onderzoeksVariant;
	}

	public void setOnderzoeksVariant(ColonOnderzoeksVariant onderzoeksVariant)
	{
		this.onderzoeksVariant = onderzoeksVariant;
	}

	public Date getUitgesteldeUitslagDatum()
	{
		return uitgesteldeUitslagDatum;
	}

	public void setUitgesteldeUitslagDatum(Date uitgesteldUitslagDatum)
	{
		this.uitgesteldeUitslagDatum = uitgesteldUitslagDatum;
	}

}
