package nl.rivm.screenit.service;

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

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.function.Consumer;

import nl.rivm.screenit.document.BaseDocumentCreator;
import nl.rivm.screenit.model.BezwaarMoment;
import nl.rivm.screenit.model.Brief;
import nl.rivm.screenit.model.BriefDefinitie;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientBrief;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.MergedBrieven;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.algemeen.AlgemeneBrief;
import nl.rivm.screenit.model.algemeen.BezwaarBrief;
import nl.rivm.screenit.model.batch.BvoZoekCriteria;
import nl.rivm.screenit.model.cervix.CervixAfmelding;
import nl.rivm.screenit.model.cervix.CervixBrief;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.rivm.screenit.model.cervix.CervixRegioBrief;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.colon.ColonAfmelding;
import nl.rivm.screenit.model.colon.ColonBrief;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.mamma.MammaAfmelding;
import nl.rivm.screenit.model.mamma.MammaBrief;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.project.ProjectBrief;
import nl.rivm.screenit.model.project.ProjectBriefActie;
import nl.rivm.screenit.model.project.ProjectClient;
import nl.rivm.screenit.service.impl.IBrievenGeneratorHelper;

import com.aspose.words.Document;

public interface BaseBriefService
{
	BriefDefinitie getBriefDefinitie(BriefType briefType, Date geldigOp);

	BriefDefinitie getNieuwsteBriefDefinitie(BriefType briefType);

	public List<BriefDefinitie> getBriefDefinities(BvoZoekCriteria briefType, Comparator<BriefType> comparator);

	void saveBriefDefinitie(BriefDefinitie definitie, File uploadFile, String contentType, String filename) throws IOException;

	ColonBrief maakColonBrief(ColonScreeningRonde ronde, BriefType type);

	ColonBrief maakColonBrief(ColonScreeningRonde ronde, BriefType type, Date date);

	ColonBrief maakColonBrief(ColonAfmelding afmelding, BriefType type, Date date);

	ColonBrief maakColonBrief(Client client, BriefType type, Date date);

	BezwaarBrief maakBezwaarBrief(BezwaarMoment bezwaar, BriefType type, Date date);

	AlgemeneBrief maakAlgemeneBrief(Client client, BriefType type);

	ProjectBrief maakProjectBrief(ProjectClient pClient, ProjectBriefActie actie);

	CervixBrief maakCervixBrief(CervixScreeningRonde ronde, BriefType type);

	CervixBrief maakCervixBrief(CervixScreeningRonde ronde, BriefType type, Date date);

	CervixBrief maakCervixBrief(CervixAfmelding afmelding, BriefType type, Date date);

	CervixBrief maakCervixBrief(Client client, BriefType type, Date date);

	CervixRegioBrief maakRegioBrief(ScreeningOrganisatie so, BriefType type, Date date, CervixHuisarts arts);

	<B extends ClientBrief<?, ?, ?>> void checkVoorDubbeleBrieven(BriefType type, Client client, Class<B> briefClass);

	boolean clientHeeftOngegenereerdeBriefVanType(BriefType type, Client client, Class<? extends ClientBrief> briefClass);

	MammaBrief maakMammaBrief(MammaScreeningRonde ronde, BriefType briefType);

	MammaBrief maakMammaBrief(MammaScreeningRonde ronde, BriefType type, Date date);

	MammaBrief maakMammaBrief(MammaScreeningRonde ronde, BriefType briefType, boolean briefGegenereerd);

	MammaBrief maakMammaBrief(MammaScreeningRonde ronde, BriefType type, Date date, boolean briefGegenereerd);

	MammaBrief maakMammaBrief(MammaAfmelding afmelding, BriefType type, Date date);

	MammaBrief maakMammaBrief(Client client, BriefType type, Date date);

	FileOutputStream completeEnGetPdf(MergedBrieven<?> mergedBrieven) throws IOException;

	<B extends Brief, MB extends MergedBrieven<?>> void createOrAddMergedBrieven(List<? extends B> items, IBrievenGeneratorHelper<B, MB> briefGenerator) throws Exception;

	<B extends Brief> File maakPdfVanBrief(B brief) throws Exception;

	<B extends Brief> File maakPdfVanBrief(B brief, BaseDocumentCreator documentCreator) throws Exception;

	<B extends Brief> File maakPdfVanBrief(B brief, BaseDocumentCreator documentCreator, Consumer<MailMergeContext> mergeContextConsumer) throws Exception;

	<B extends Brief> File maakPdfVanBrief(MammaBrief brief, Consumer<MailMergeContext> context) throws Exception;

	File genereerPdf(Document document, String fileNaam, boolean autoShowPrintdialog) throws FileNotFoundException, Exception;

	<B extends Brief> void setBriefGegenereerd(B brief);

	void setNietGegenereerdeBrievenOpTegenhouden(MammaScreeningRonde screeningRonde, BriefType briefType);

	List<ClientBrief> getClientBrieven(Client client);

}
