package nl.rivm.screenit.model;

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

import nl.rivm.screenit.model.colon.enums.RetourzendingStatus;
import nl.rivm.screenit.model.colon.enums.RetourzendingWijze;
import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import java.util.Date;

@Entity
@Table
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
@Audited
public abstract class InpakbareUitnodiging<SR extends ScreeningRonde<?, ?, ?, ?>> extends Uitnodiging<SR>
{
    @Column(nullable = false)
    private Long uitnodigingsId;

    private boolean verstuurd;

    private boolean verstuurdDoorInpakcentrum;

    @Deprecated
    @Temporal(TemporalType.TIMESTAMP)
    private Date datumTerugOntvangen;

    @Temporal(TemporalType.TIMESTAMP)
    private Date retourOntvangen;

    private String retourzendingReden;

    @Enumerated(EnumType.STRING)
    private RetourzendingStatus retourzendingStatus;

    @Enumerated(EnumType.STRING)
    private RetourzendingWijze retourzendingWijze;

    private String templateNaam;

    private String trackTraceId;

    @Temporal(TemporalType.TIMESTAMP)
    private Date verstuurdDatum;

    public Long getUitnodigingsId()
    {
        return uitnodigingsId;
    }

    public void setUitnodigingsId(Long uitnodigingsId)
    {
        this.uitnodigingsId = uitnodigingsId;
    }

    public boolean isVerstuurd()
    {
        return verstuurd;
    }

    public void setVerstuurd(boolean verstuurd)
    {
        this.verstuurd = verstuurd;
    }

    public Date getVerstuurdDatum()
    {
        return verstuurdDatum;
    }

    public void setVerstuurdDatum(Date verstuurdDatum)
    {
        this.verstuurdDatum = verstuurdDatum;
    }

    public boolean isVerstuurdDoorInpakcentrum()
    {
        return verstuurdDoorInpakcentrum;
    }

    public void setVerstuurdDoorInpakcentrum(boolean verstuurdDoorInpakcentrum)
    {
        this.verstuurdDoorInpakcentrum = verstuurdDoorInpakcentrum;
    }

    public Date getDatumTerugOntvangen()
    {
        return datumTerugOntvangen;
    }

    public void setDatumTerugOntvangen(Date datumTerugOntvangen)
    {
        this.datumTerugOntvangen = datumTerugOntvangen;
    }

    public String getTemplateNaam()
    {
        return templateNaam;
    }

    public void setTemplateNaam(String templateNaam)
    {
        this.templateNaam = templateNaam;
    }

    public String getTrackTraceId()
    {
        return trackTraceId;
    }

    public void setTrackTraceId(String trackTraceId)
    {
        this.trackTraceId = trackTraceId;
    }

    public String getRetourzendingReden()
    {
        return retourzendingReden;
    }

    public void setRetourzendingReden(String retourzendingReden)
    {
        this.retourzendingReden = retourzendingReden;
    }

    public RetourzendingStatus getRetourzendingStatus()
    {
        return retourzendingStatus;
    }

    public void setRetourzendingStatus(RetourzendingStatus retourzendingStatus)
    {
        this.retourzendingStatus = retourzendingStatus;
    }

    public Date getRetourOntvangen()
    {
        return retourOntvangen;
    }

    public void setRetourOntvangen(Date retourOntvangen)
    {
        this.retourOntvangen = retourOntvangen;
    }

    public RetourzendingWijze getRetourzendingWijze()
    {
        return retourzendingWijze;
    }

    public void setRetourzendingWijze(RetourzendingWijze retourzendingWijze)
    {
        this.retourzendingWijze = retourzendingWijze;
    }
}
