package nl.rivm.screenit.model.colon.verslag.mdl;

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

import javax.annotation.Nonnull;
import javax.persistence.AttributeOverride;
import javax.persistence.AttributeOverrides;
import javax.persistence.Column;
import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import nl.rivm.screenit.model.verslag.DSValue;
import nl.rivm.screenit.model.verslag.DSValueSet;
import nl.rivm.screenit.model.verslag.DSValueSetValue;
import nl.rivm.screenit.model.verslag.Quantity;
import nl.rivm.screenit.model.verslag.VraagElement;
import nl.rivm.screenit.model.verslag.VraagElementUnit;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import com.fasterxml.jackson.annotation.JsonIgnore;

@Entity
@Table(schema = "colon")
public class MdlDefinitiefVervolgbeleidVoorBevolkingsonderzoekg
	extends AbstractHibernateObject
{

	private final static long serialVersionUID = 1L;

	@ManyToOne(fetch = FetchType.LAZY)
	@JsonIgnore
	private MdlColoscopieMedischeObservatie coloscopieMedischeObservatie;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_vervolgbeleid", values = {
		@DSValueSetValue(code = "183851006", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "311774002", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "410410006", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "308535007", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "73761001", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "73761001:260870009=64695001", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "418714002", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "428119001:363589002=73761001", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "183654001", codeSystem = "2.16.840.1.113883.6.96", deprecated = true),
		@DSValueSetValue(code = "73761001:408730004=64695001", codeSystem = "2.16.840.1.113883.6.96", deprecated = true),
		@DSValueSetValue(code = "410410006:408730004=428119001", codeSystem = "2.16.840.1.113883.6.96", deprecated = true)
	})
	@VraagElement(displayName = "Definitief vervolgbeleid voor bevolkingsonderzoek", extraTekst = "Definitief te voeren vervolgbeleid vanuit het perspectief van het bevolkingsonderzoek.\u00a0\u00a0De waarde in dit veld moet consistent zijn met de eindconclusie.", code = "2.16.840.1.113883.2.4.3.36.77.2.11.110", isVerplicht = true)
	private DSValue definitiefVervolgbeleidVoorBevolkingsonderzoek;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_periode_vervolg", values = {
		@DSValueSetValue(code = "1", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.226"),
		@DSValueSetValue(code = "2", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.226"),
		@DSValueSetValue(code = "3", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.226"),
		@DSValueSetValue(code = "4", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.226"),
		@DSValueSetValue(code = "5", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.226"),
		@DSValueSetValue(code = "6", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.226"),
		@DSValueSetValue(code = "7", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.226"),
		@DSValueSetValue(code = "8", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.226"),
		@DSValueSetValue(code = "9", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.226"),
		@DSValueSetValue(code = "10", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.226"),
		@DSValueSetValue(code = "11", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.226"),
		@DSValueSetValue(code = "12", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.226"),
		@DSValueSetValue(code = "13", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.226", deprecated = true),
		@DSValueSetValue(code = "14", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.226"),
		@DSValueSetValue(code = "15", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.226", deprecated = true)
	})
	@VraagElement(displayName = "Periode vervolg surveillance/scopie", extraTekst = "Periode waarna pati\u00ebnt weer terugkomt terug voor surveillance/vervolgcoloscopie. Bij screening wordt het interval elders bepaald en mag niet ingevuld worden.", code = "2.16.840.1.113883.2.4.3.36.77.2.11.111", useInCda = false)
	private DSValue periodeVervolgSurveillancescopie;

	@VraagElement(code = "2.16.840.1.113883.2.4.3.36.77.2.11.111", displayName = "", useInFormulier = false)
	@Embedded
	@Nonnull
	@AttributeOverrides({
		@AttributeOverride(name = "value", column = @Column(name = "periodeVervolgSurveillancecoloscopieCdaValue")),
		@AttributeOverride(name = "unit", column = @Column(name = "periodeVervolgSurveillancecoloscopieCdaUnit"))
	})
	private Quantity periodeVervolgSurveillancecoloscopieCda;

	@Column(length = 4096)
	@VraagElement(displayName = "Locatie vervolgscopie", extraTekst = "Aanduiding naar welk centrum de pati\u00ebnt is/wordt verwezen", code = "2.16.840.1.113883.2.4.3.36.77.2.11.112")
	private String locatieVervolgscopie;

	@Embedded
	@Nonnull
	@AttributeOverrides({
		@AttributeOverride(name = "value", column = @Column(name = "adenoomRiskScoretotalValue")),
		@AttributeOverride(name = "unit", column = @Column(name = "adenoomRiskScoretotalUnit"))
	})
	@VraagElement(displayName = "Adenoom risk score (total)", extraTekst = "", code = "2.16.840.1.113883.2.4.3.36.77.2.11.113", unit = {
		@VraagElementUnit(unit = "", min = "0.0", max = "5.0")
	})
	private Quantity adenoomRiskScoretotal;

	public MdlColoscopieMedischeObservatie getColoscopieMedischeObservatie()
	{
		return coloscopieMedischeObservatie;
	}

	public void setColoscopieMedischeObservatie(MdlColoscopieMedischeObservatie coloscopieMedischeObservatie)
	{
		this.coloscopieMedischeObservatie = coloscopieMedischeObservatie;
	}

	public DSValue getDefinitiefVervolgbeleidVoorBevolkingsonderzoek()
	{
		return definitiefVervolgbeleidVoorBevolkingsonderzoek;
	}

	public void setDefinitiefVervolgbeleidVoorBevolkingsonderzoek(DSValue definitiefVervolgbeleidVoorBevolkingsonderzoek)
	{
		this.definitiefVervolgbeleidVoorBevolkingsonderzoek = definitiefVervolgbeleidVoorBevolkingsonderzoek;
	}

	public Quantity getPeriodeVervolgSurveillancecoloscopieCda()
	{
		return periodeVervolgSurveillancecoloscopieCda;
	}

	public void setPeriodeVervolgSurveillancecoloscopieCda(Quantity periodeVervolgSurveillancecoloscopieCda)
	{
		this.periodeVervolgSurveillancecoloscopieCda = periodeVervolgSurveillancecoloscopieCda;
	}

	public DSValue getPeriodeVervolgSurveillancescopie()
	{
		return periodeVervolgSurveillancescopie;
	}

	public void setPeriodeVervolgSurveillancescopie(DSValue periodeVervolgSurveillancescopie)
	{
		this.periodeVervolgSurveillancescopie = periodeVervolgSurveillancescopie;
	}

	public String getLocatieVervolgscopie()
	{
		return locatieVervolgscopie;
	}

	public void setLocatieVervolgscopie(String locatieVervolgscopie)
	{
		this.locatieVervolgscopie = locatieVervolgscopie;
	}

	public Quantity getAdenoomRiskScoretotal()
	{
		return adenoomRiskScoretotal;
	}

	public void setAdenoomRiskScoretotal(Quantity adenoomRiskScoretotal)
	{
		this.adenoomRiskScoretotal = adenoomRiskScoretotal;
	}

}
