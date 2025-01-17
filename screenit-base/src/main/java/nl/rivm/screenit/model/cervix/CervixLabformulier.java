package nl.rivm.screenit.model.cervix;

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

import javax.annotation.CheckForNull;
import javax.persistence.CollectionTable;
import javax.persistence.Column;
import javax.persistence.ElementCollection;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.Index;
import javax.persistence.ManyToOne;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import javax.persistence.UniqueConstraint;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.ScannedFormulier;
import nl.rivm.screenit.model.cervix.enums.CervixLabformulierStatus;
import nl.rivm.screenit.model.cervix.enums.signaleringen.CervixLabformulierSignalering;
import nl.rivm.screenit.util.DiffSpecs;
import nl.rivm.screenit.util.SkipFieldForDiff;

import org.hibernate.envers.Audited;

@Getter
@Setter
@Entity
@Table(
	schema = "cervix",
	name = "labformulier",
	uniqueConstraints = { @UniqueConstraint(name = "uc_clf_objid", columnNames = "objid"),
		@UniqueConstraint(name = "uc_clf_huisarts_onbekend_brief", columnNames = "huisarts_onbekend_brief"),
		@UniqueConstraint(name = "uc_clf_uitstrijkje_ontbreekt_huisarts_bericht", columnNames = "uitstrijkje_ontbreekt_huisarts_bericht") },
	indexes = {
		@Index(name = "idx_CERVIX_LABFORMULIER_STATUS", columnList = "status"), @Index(name = "idx_CERVIX_LABFORMULIER_SCAN_DATUM", columnList = "scanDatum"),
		@Index(name = "idx_CERVIX_LABFORMULIER_BARCODE", columnList = "barcode"), @Index(name = "idx_CERVIX_LABFORMULIER_DATUM_GEWIST", columnList = "datumGewist") })
@Audited
public class CervixLabformulier extends ScannedFormulier
{

	@ManyToOne(optional = false, fetch = FetchType.LAZY)
	private BMHKLaboratorium laboratorium;

	@OneToOne(mappedBy = "labformulier", fetch = FetchType.LAZY)
	@SkipFieldForDiff
	private CervixUitstrijkje uitstrijkje;

	@ManyToOne(fetch = FetchType.LAZY)
	@DiffSpecs(displayProperty = "naam")
	private CervixHuisartsLocatie huisartsLocatie;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private CervixLabformulierStatus status;

	@Column(nullable = false)
	@Temporal(TemporalType.TIMESTAMP)
	private Date statusDatum;

	@OneToOne(fetch = FetchType.LAZY)
	private CervixBrief huisartsOnbekendBrief;

	@OneToOne(fetch = FetchType.LAZY)
	private CervixHuisartsBericht uitstrijkjeOntbreektHuisartsBericht;

	@Column(nullable = false)
	private Boolean kunstmatig = Boolean.FALSE;

	@Column
	private boolean digitaal = false;

	@Temporal(TemporalType.DATE)
	private Date datumUitstrijkje;

	@Temporal(TemporalType.TIMESTAMP)
	private Date datumGewist;

	@ElementCollection(targetClass = CervixLabformulierSignalering.class)
	@Column
	@Enumerated(EnumType.STRING)
	@CollectionTable(schema = "cervix", name = "labformulier_signalering")
	private List<CervixLabformulierSignalering> signaleringen = new ArrayList<>();

	@Column
	private String overigeLabformulierSignalering;

	@SkipFieldForDiff
	private Boolean klachtenGeen = false;

	@SkipFieldForDiff
	private Boolean klachtenContactbloedingen = false;

	@SkipFieldForDiff
	private Boolean klachtenKlachtenVanAbnormaleFluorZonderDuidelijkeOorzaak = false;

	@SkipFieldForDiff
	private Boolean klachtenIntermenstrueelBloedverlies = false;

	@SkipFieldForDiff
	private Boolean klachtenPostmenopauzaalBloedverlies = false;

	@SkipFieldForDiff
	private Boolean klachtenAndersNamelijk = false;

	@Column(length = 240)
	@SkipFieldForDiff
	private String klachtenAndersNamelijkTekst;

	@SkipFieldForDiff
	private Boolean menstruatieNormaal = false;

	@SkipFieldForDiff
	private Boolean menstruatieGeenMenstruatie = false;

	@SkipFieldForDiff
	private Boolean menstruatieMenopauze = false;

	@SkipFieldForDiff
	private Boolean menstruatiePostmenopauze = false;

	@Temporal(TemporalType.DATE)
	@SkipFieldForDiff
	private Date datumLaatsteMenstruatie;

	@SkipFieldForDiff
	private Boolean anticonceptieGeen = false;

	@SkipFieldForDiff
	private Boolean anticonceptiePil = false;

	@SkipFieldForDiff
	private Boolean anticonceptieIudKoper = false;

	@SkipFieldForDiff
	private Boolean anticonceptieIudMirena = false;

	@SkipFieldForDiff
	private Boolean anticonceptieAnders = false;

	@SkipFieldForDiff
	private Boolean gebruikHormonenJaVanwegeOvergangsklachten = false;

	@SkipFieldForDiff
	private Boolean gebruikHormonenJaVanwegeBorstkanker = false;

	@SkipFieldForDiff
	private Boolean gebruikHormonenJaVanwege = false;

	@Column(length = 240)
	@SkipFieldForDiff
	private String gebruikHormonenJaVanwegeTekst;

	@SkipFieldForDiff
	private Boolean gebruikHormonenGeen = false;

	@SkipFieldForDiff
	private Boolean aspectCervixNietGezien = false;

	@SkipFieldForDiff
	private Boolean aspectCervixAbnormaalOfVerdachtePortio = false;

	@Column(length = 240)
	@SkipFieldForDiff
	private String aspectCervixAbnormaalOfVerdachtePortioTekst;

	@SkipFieldForDiff
	private Boolean aspectCervixNormaal = false;

	@SkipFieldForDiff
	private Boolean opmerkingen = false;

	@Column(length = 240)
	@SkipFieldForDiff
	private String opmerkingenTekst;

	@Column(length = 255)
	@SkipFieldForDiff
	private String leverancierFqdn;

	public BMHKLaboratorium getLaboratorium()
	{
		return laboratorium;
	}

	public void setLaboratorium(BMHKLaboratorium laboratorium)
	{
		this.laboratorium = laboratorium;
	}

	@CheckForNull
	public CervixUitstrijkje getUitstrijkje()
	{
		return uitstrijkje;
	}

	public void setUitstrijkje(CervixUitstrijkje uitstrijkje)
	{
		this.uitstrijkje = uitstrijkje;
	}

	public CervixLabformulierStatus getStatus()
	{
		return status;
	}

	public void setStatus(CervixLabformulierStatus labFormulierStatus)
	{
		this.status = labFormulierStatus;
	}

	public Date getStatusDatum()
	{
		return statusDatum;
	}

	public void setStatusDatum(Date labFormulierStatusDatum)
	{
		this.statusDatum = labFormulierStatusDatum;
	}

	public Boolean getKunstmatig()
	{
		return kunstmatig;
	}

	public void setKunstmatig(Boolean kunstmatig)
	{
		this.kunstmatig = kunstmatig;
	}

	public Boolean getDigitaal()
	{
		return digitaal;
	}

	public void setDigitaal(Boolean digitaal)
	{
		this.digitaal = digitaal;
	}

	public Date getDatumUitstrijkje()
	{
		return datumUitstrijkje;
	}

	public void setDatumUitstrijkje(Date datumUitstrijkje)
	{
		this.datumUitstrijkje = datumUitstrijkje;
	}

	public CervixHuisartsLocatie getHuisartsLocatie()
	{
		return huisartsLocatie;
	}

	public void setHuisartsLocatie(CervixHuisartsLocatie huisartsLocatie)
	{
		this.huisartsLocatie = huisartsLocatie;
	}

	public Boolean isKlachtenGeen()
	{
		return klachtenGeen;
	}

	public void setKlachtenGeen(Boolean klachtenGeen)
	{
		this.klachtenGeen = klachtenGeen;
	}

	public Boolean isKlachtenContactbloedingen()
	{
		return klachtenContactbloedingen;
	}

	public void setKlachtenContactbloedingen(Boolean klachtenContactbloedingen)
	{
		this.klachtenContactbloedingen = klachtenContactbloedingen;
	}

	public Boolean isKlachtenKlachtenVanAbnormaleFluorZonderDuidelijkeOorzaak()
	{
		return klachtenKlachtenVanAbnormaleFluorZonderDuidelijkeOorzaak;
	}

	public void setKlachtenKlachtenVanAbnormaleFluorZonderDuidelijkeOorzaak(Boolean klachtenKlachtenVanAbnormaleFluorZonderDuidelijkeOorzaak)
	{
		this.klachtenKlachtenVanAbnormaleFluorZonderDuidelijkeOorzaak = klachtenKlachtenVanAbnormaleFluorZonderDuidelijkeOorzaak;
	}

	public Boolean isKlachtenIntermenstrueelBloedverlies()
	{
		return klachtenIntermenstrueelBloedverlies;
	}

	public void setKlachtenIntermenstrueelBloedverlies(Boolean klachtenIntermenstrueelBloedverlies)
	{
		this.klachtenIntermenstrueelBloedverlies = klachtenIntermenstrueelBloedverlies;
	}

	public Boolean isKlachtenPostmenopauzaalBloedverlies()
	{
		return klachtenPostmenopauzaalBloedverlies;
	}

	public void setKlachtenPostmenopauzaalBloedverlies(Boolean klachtenPostmenopauzaalBloedverlies)
	{
		this.klachtenPostmenopauzaalBloedverlies = klachtenPostmenopauzaalBloedverlies;
	}

	public Boolean isKlachtenAndersNamelijk()
	{
		return klachtenAndersNamelijk;
	}

	public void setKlachtenAndersNamelijk(Boolean klachtenAndersNamelijk)
	{
		this.klachtenAndersNamelijk = klachtenAndersNamelijk;
	}

	public String getKlachtenAndersNamelijkTekst()
	{
		return klachtenAndersNamelijkTekst;
	}

	public void setKlachtenAndersNamelijkTekst(String klachtenAndersNamelijkTekst)
	{
		this.klachtenAndersNamelijkTekst = klachtenAndersNamelijkTekst;
	}

	public Boolean isMenstruatieNormaal()
	{
		return menstruatieNormaal;
	}

	public void setMenstruatieNormaal(Boolean menstruatieNormaal)
	{
		this.menstruatieNormaal = menstruatieNormaal;
	}

	public Boolean isMenstruatieGeenMenstruatie()
	{
		return menstruatieGeenMenstruatie;
	}

	public void setMenstruatieGeenMenstruatie(Boolean menstruatieGeenMenstruatie)
	{
		this.menstruatieGeenMenstruatie = menstruatieGeenMenstruatie;
	}

	public Boolean isMenstruatieMenopauze()
	{
		return menstruatieMenopauze;
	}

	public void setMenstruatieMenopauze(Boolean menstruatieMenopauze)
	{
		this.menstruatieMenopauze = menstruatieMenopauze;
	}

	public Boolean isMenstruatiePostmenopauze()
	{
		return menstruatiePostmenopauze;
	}

	public void setMenstruatiePostmenopauze(Boolean menstruatiePostmenopauze)
	{
		this.menstruatiePostmenopauze = menstruatiePostmenopauze;
	}

	public Date getDatumLaatsteMenstruatie()
	{
		return datumLaatsteMenstruatie;
	}

	public void setDatumLaatsteMenstruatie(Date datumLaatsteMenstruatie)
	{
		this.datumLaatsteMenstruatie = datumLaatsteMenstruatie;
	}

	public Boolean isAnticonceptieGeen()
	{
		return anticonceptieGeen;
	}

	public void setAnticonceptieGeen(Boolean anticonceptieGeen)
	{
		this.anticonceptieGeen = anticonceptieGeen;
	}

	public Boolean isAnticonceptiePil()
	{
		return anticonceptiePil;
	}

	public void setAnticonceptiePil(Boolean anticonceptiePil)
	{
		this.anticonceptiePil = anticonceptiePil;
	}

	public Boolean isAnticonceptieIudKoper()
	{
		return anticonceptieIudKoper;
	}

	public void setAnticonceptieIudKoper(Boolean anticonceptieIudKoper)
	{
		this.anticonceptieIudKoper = anticonceptieIudKoper;
	}

	public Boolean isAnticonceptieIudMirena()
	{
		return anticonceptieIudMirena;
	}

	public void setAnticonceptieIudMirena(Boolean anticonceptieIudMirena)
	{
		this.anticonceptieIudMirena = anticonceptieIudMirena;
	}

	public Boolean isAnticonceptieAnders()
	{
		return anticonceptieAnders;
	}

	public void setAnticonceptieAnders(Boolean anticonceptieAnders)
	{
		this.anticonceptieAnders = anticonceptieAnders;
	}

	public Boolean isGebruikHormonenJaVanwegeOvergangsklachten()
	{
		return gebruikHormonenJaVanwegeOvergangsklachten;
	}

	public void setGebruikHormonenJaVanwegeOvergangsklachten(Boolean gebruikHormonenJaVanwegeOvergangsklachten)
	{
		this.gebruikHormonenJaVanwegeOvergangsklachten = gebruikHormonenJaVanwegeOvergangsklachten;
	}

	public Boolean isGebruikHormonenJaVanwegeBorstkanker()
	{
		return gebruikHormonenJaVanwegeBorstkanker;
	}

	public void setGebruikHormonenJaVanwegeBorstkanker(Boolean gebruikHormonenJaVanwegeBorstkanker)
	{
		this.gebruikHormonenJaVanwegeBorstkanker = gebruikHormonenJaVanwegeBorstkanker;
	}

	public Boolean isGebruikHormonenJaVanwege()
	{
		return gebruikHormonenJaVanwege;
	}

	public void setGebruikHormonenJaVanwege(Boolean gebruikHormonenJaVanwege)
	{
		this.gebruikHormonenJaVanwege = gebruikHormonenJaVanwege;
	}

	public String getGebruikHormonenJaVanwegeTekst()
	{
		return gebruikHormonenJaVanwegeTekst;
	}

	public void setGebruikHormonenJaVanwegeTekst(String gebruikHormonenJaVanwegeTekst)
	{
		this.gebruikHormonenJaVanwegeTekst = gebruikHormonenJaVanwegeTekst;
	}

	public Boolean isGebruikHormonenGeen()
	{
		return gebruikHormonenGeen;
	}

	public void setGebruikHormonenGeen(Boolean gebruikHormonenGeen)
	{
		this.gebruikHormonenGeen = gebruikHormonenGeen;
	}

	public Boolean isAspectCervixNietGezien()
	{
		return aspectCervixNietGezien;
	}

	public void setAspectCervixNietGezien(Boolean aspectCervixNietGezien)
	{
		this.aspectCervixNietGezien = aspectCervixNietGezien;
	}

	public Boolean isAspectCervixNormaal()
	{
		return aspectCervixNormaal;
	}

	public void setAspectCervixNormaal(Boolean aspectCervixNormaal)
	{
		this.aspectCervixNormaal = aspectCervixNormaal;
	}

	public Boolean isAspectCervixAbnormaalOfVerdachtePortio()
	{
		return aspectCervixAbnormaalOfVerdachtePortio;
	}

	public void setAspectCervixAbnormaalOfVerdachtePortio(Boolean aspectCervixAbnormaalOfVerdachtePortio)
	{
		this.aspectCervixAbnormaalOfVerdachtePortio = aspectCervixAbnormaalOfVerdachtePortio;
	}

	public String getAspectCervixAbnormaalOfVerdachtePortioTekst()
	{
		return aspectCervixAbnormaalOfVerdachtePortioTekst;
	}

	public void setAspectCervixAbnormaalOfVerdachtePortioTekst(String aspectCervixAbnormaalOfVerdachtePortioTekst)
	{
		this.aspectCervixAbnormaalOfVerdachtePortioTekst = aspectCervixAbnormaalOfVerdachtePortioTekst;
	}

	public Boolean isOpmerkingen()
	{
		return opmerkingen;
	}

	public void setOpmerkingen(Boolean opmerkingen)
	{
		this.opmerkingen = opmerkingen;
	}

	public String getOpmerkingenTekst()
	{
		return opmerkingenTekst;
	}

	public void setOpmerkingenTekst(String opmerkingenTekst)
	{
		this.opmerkingenTekst = opmerkingenTekst;
	}

	public CervixBrief getHuisartsOnbekendBrief()
	{
		return huisartsOnbekendBrief;
	}

	public void setHuisartsOnbekendBrief(CervixBrief huisartsOnbekendBrief)
	{
		this.huisartsOnbekendBrief = huisartsOnbekendBrief;
	}

	public CervixHuisartsBericht getUitstrijkjeOntbreektHuisartsBericht()
	{
		return uitstrijkjeOntbreektHuisartsBericht;
	}

	public void setUitstrijkjeOntbreektHuisartsBericht(CervixHuisartsBericht uitstrijkjeOntbreektHuisartsBericht)
	{
		this.uitstrijkjeOntbreektHuisartsBericht = uitstrijkjeOntbreektHuisartsBericht;
	}

	public Date getDatumGewist()
	{
		return datumGewist;
	}

	public void setDatumGewist(Date datumGewist)
	{
		this.datumGewist = datumGewist;
	}

	public List<CervixLabformulierSignalering> getSignaleringen()
	{
		return signaleringen;
	}

	public void setSignaleringen(List<CervixLabformulierSignalering> afwijkingen)
	{
		this.signaleringen = afwijkingen;
	}

	public String getOverigeLabformulierSignalering()
	{
		return overigeLabformulierSignalering;
	}

	public void setOverigeLabformulierSignalering(String overigeLabformulierSignalering)
	{
		this.overigeLabformulierSignalering = overigeLabformulierSignalering;
	}
}
