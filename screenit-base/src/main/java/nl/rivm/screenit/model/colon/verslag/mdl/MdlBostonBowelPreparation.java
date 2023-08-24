package nl.rivm.screenit.model.colon.verslag.mdl;

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

import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import nl.rivm.screenit.model.verslag.DSValue;
import nl.rivm.screenit.model.verslag.DSValueSet;
import nl.rivm.screenit.model.verslag.DSValueSetValue;
import nl.rivm.screenit.model.verslag.VraagElement;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import com.fasterxml.jackson.annotation.JsonIgnore;

@Entity
@Table(schema = "colon")
public class MdlBostonBowelPreparation
	extends AbstractHibernateObject
{

	private final static long serialVersionUID = 1L;

	@ManyToOne(fetch = FetchType.LAZY)
	@JsonIgnore
	private MdlVoorbereidingColoscopie voorbereidingColoscopie;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_BBPS_sumscore", values = {
		@DSValueSetValue(code = "0", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.103"),
		@DSValueSetValue(code = "1", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.103"),
		@DSValueSetValue(code = "2", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.103"),
		@DSValueSetValue(code = "3", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.103"),
		@DSValueSetValue(code = "4", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.103"),
		@DSValueSetValue(code = "5", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.103"),
		@DSValueSetValue(code = "6", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.103"),
		@DSValueSetValue(code = "7", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.103"),
		@DSValueSetValue(code = "8", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.103"),
		@DSValueSetValue(code = "9", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.103"),
		@DSValueSetValue(code = "ASKU", codeSystem = "2.16.840.1.113883.5.1008")
	})
	@VraagElement(displayName = "Boston Bowel Preparation Scale Sum Score", extraTekst = "Boston Bowel Preparation Scale Sum Score", code = "2.16.840.1.113883.2.4.3.36.77.2.11.85", isVerplicht = true)
	private DSValue bostonBowelPreparationScaleSumScore;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_BBPS_xscale", values = {
		@DSValueSetValue(code = "0", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.102"),
		@DSValueSetValue(code = "1", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.102"),
		@DSValueSetValue(code = "2", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.102"),
		@DSValueSetValue(code = "3", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.102"),
		@DSValueSetValue(code = "ASKU", codeSystem = "2.16.840.1.113883.5.1008")
	})
	@VraagElement(displayName = "BBPS Score Colon ascendens", extraTekst = "Boston Bowel Preparation Scale (score: 0-3) Colon ascendens", code = "2.16.840.1.113883.2.4.3.36.77.2.11.86", isVerplicht = true)
	private DSValue bbpsScoreColonAscendens;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_BBPS_xscale", values = {
		@DSValueSetValue(code = "0", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.102"),
		@DSValueSetValue(code = "1", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.102"),
		@DSValueSetValue(code = "2", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.102"),
		@DSValueSetValue(code = "3", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.102"),
		@DSValueSetValue(code = "ASKU", codeSystem = "2.16.840.1.113883.5.1008")
	})
	@VraagElement(displayName = "BBPS Score Colon transversum", extraTekst = "Boston Bowel Preparation Scale (score: 0-3) Colon transversum", code = "2.16.840.1.113883.2.4.3.36.77.2.11.87", isVerplicht = true)
	private DSValue bbpsScoreColonTransversum;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_BBPS_xscale", values = {
		@DSValueSetValue(code = "0", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.102"),
		@DSValueSetValue(code = "1", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.102"),
		@DSValueSetValue(code = "2", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.102"),
		@DSValueSetValue(code = "3", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.102"),
		@DSValueSetValue(code = "ASKU", codeSystem = "2.16.840.1.113883.5.1008")
	})
	@VraagElement(displayName = "BBPS Score Colon descendens", extraTekst = "Boston Bowel Preparation Scale (score: 0-3) Colon descendens", code = "2.16.840.1.113883.2.4.3.36.77.2.11.88", isVerplicht = true)
	private DSValue bbpsScoreColonDescendens;

	public MdlVoorbereidingColoscopie getVoorbereidingColoscopie()
	{
		return voorbereidingColoscopie;
	}

	public void setVoorbereidingColoscopie(MdlVoorbereidingColoscopie voorbereidingColoscopie)
	{
		this.voorbereidingColoscopie = voorbereidingColoscopie;
	}

	public DSValue getBostonBowelPreparationScaleSumScore()
	{
		return bostonBowelPreparationScaleSumScore;
	}

	public void setBostonBowelPreparationScaleSumScore(DSValue bostonBowelPreparationScaleSumScore)
	{
		this.bostonBowelPreparationScaleSumScore = bostonBowelPreparationScaleSumScore;
	}

	public DSValue getBbpsScoreColonAscendens()
	{
		return bbpsScoreColonAscendens;
	}

	public void setBbpsScoreColonAscendens(DSValue bbpsScoreColonAscendens)
	{
		this.bbpsScoreColonAscendens = bbpsScoreColonAscendens;
	}

	public DSValue getBbpsScoreColonTransversum()
	{
		return bbpsScoreColonTransversum;
	}

	public void setBbpsScoreColonTransversum(DSValue bbpsScoreColonTransversum)
	{
		this.bbpsScoreColonTransversum = bbpsScoreColonTransversum;
	}

	public DSValue getBbpsScoreColonDescendens()
	{
		return bbpsScoreColonDescendens;
	}

	public void setBbpsScoreColonDescendens(DSValue bbpsScoreColonDescendens)
	{
		this.bbpsScoreColonDescendens = bbpsScoreColonDescendens;
	}

}
