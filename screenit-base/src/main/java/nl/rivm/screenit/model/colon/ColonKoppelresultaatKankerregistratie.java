
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

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import nl.rivm.screenit.model.AbstractKoppelresultaatKankerregistratie;
import nl.rivm.screenit.model.UploadDocument;

import org.hibernate.annotations.Cascade;
import org.hibernate.annotations.CascadeType;

@Entity
@Table(schema = "colon", name = "koppelresultaat_kankerregistratie")
public class ColonKoppelresultaatKankerregistratie extends AbstractKoppelresultaatKankerregistratie<ColonScreeningRonde>
{
	
	private static final long serialVersionUID = 1L;

	@Temporal(TemporalType.DATE)
	@Column(nullable = false)
	private Date incidentiedatum;

	@Column(nullable = false)
	private Integer eid;

	private String codeZiekenhuisVanDiagnose;

	private String omschrijvingZiekenhuisVanDiagnose;

	private String redenDiagnose;

	private String topografie;

	private String morfologie;

	private String tumorgedrag;

	private String differentiatiegraad;

	private String cTNM;

	private String pTNM;

	private String ypTNM;

	@ManyToOne(optional = false)
	@Cascade(CascadeType.SAVE_UPDATE)
	private UploadDocument uploadedFile;

	@ManyToOne(optional = false)
	@Cascade(CascadeType.SAVE_UPDATE)
	private ColonScreeningRonde screeningsRonde;

	public Date getIncidentiedatum()
	{
		return incidentiedatum;
	}

	public void setIncidentiedatum(Date incidentiedatum)
	{
		this.incidentiedatum = incidentiedatum;
	}

	public Integer getEid()
	{
		return eid;
	}

	public void setEid(Integer eid)
	{
		this.eid = eid;
	}

	public String getCodeZiekenhuisVanDiagnose()
	{
		return codeZiekenhuisVanDiagnose;
	}

	public void setCodeZiekenhuisVanDiagnose(String codeZiekenhuisVanDiagnose)
	{
		this.codeZiekenhuisVanDiagnose = codeZiekenhuisVanDiagnose;
	}

	public String getOmschrijvingZiekenhuisVanDiagnose()
	{
		return omschrijvingZiekenhuisVanDiagnose;
	}

	public void setOmschrijvingZiekenhuisVanDiagnose(String omschrijvingZiekenhuisVanDiagnose)
	{
		this.omschrijvingZiekenhuisVanDiagnose = omschrijvingZiekenhuisVanDiagnose;
	}

	public String getRedenDiagnose()
	{
		return redenDiagnose;
	}

	public void setRedenDiagnose(String redenDiagnose)
	{
		this.redenDiagnose = redenDiagnose;
	}

	public String getTopografie()
	{
		return topografie;
	}

	public void setTopografie(String topografie)
	{
		this.topografie = topografie;
	}

	public String getMorfologie()
	{
		return morfologie;
	}

	public void setMorfologie(String morfologie)
	{
		this.morfologie = morfologie;
	}

	public String getTumorgedrag()
	{
		return tumorgedrag;
	}

	public void setTumorgedrag(String tumorgedrag)
	{
		this.tumorgedrag = tumorgedrag;
	}

	public String getDifferentiatiegraad()
	{
		return differentiatiegraad;
	}

	public void setDifferentiatiegraad(String differentiatiegraad)
	{
		this.differentiatiegraad = differentiatiegraad;
	}

	public String getcTNM()
	{
		return cTNM;
	}

	public void setcTNM(String cTNM)
	{
		this.cTNM = cTNM;
	}

	public String getpTNM()
	{
		return pTNM;
	}

	public void setpTNM(String pTNM)
	{
		this.pTNM = pTNM;
	}

	public String getYpTNM()
	{
		return ypTNM;
	}

	public void setYpTNM(String ypTNM)
	{
		this.ypTNM = ypTNM;
	}

	@Override
	public ColonScreeningRonde getScreeningsRonde()
	{
		return screeningsRonde;
	}

	@Override
	public void setScreeningsRonde(ColonScreeningRonde screeningsRonde)
	{
		this.screeningsRonde = screeningsRonde;
	}

	public UploadDocument getUploadedFile()
	{
		return uploadedFile;
	}

	public void setUploadedFile(UploadDocument uploadedFile)
	{
		this.uploadedFile = uploadedFile;
	}

}
