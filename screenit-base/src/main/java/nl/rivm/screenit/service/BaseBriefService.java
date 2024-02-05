package nl.rivm.screenit.service;

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

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Collection;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.function.Consumer;
import java.util.function.UnaryOperator;

import nl.rivm.screenit.document.BaseDocumentCreator;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Afmelding;
import nl.rivm.screenit.model.BezwaarMoment;
import nl.rivm.screenit.model.Brief;
import nl.rivm.screenit.model.BriefDefinitie;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientBrief;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.MergedBrieven;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.ScreeningRonde;
import nl.rivm.screenit.model.algemeen.AlgemeneBrief;
import nl.rivm.screenit.model.algemeen.BezwaarBrief;
import nl.rivm.screenit.model.batch.BvoZoekCriteria;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.rivm.screenit.model.cervix.CervixRegioBrief;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.project.ProjectBrief;
import nl.rivm.screenit.model.project.ProjectBriefActie;
import nl.rivm.screenit.model.project.ProjectClient;
import nl.rivm.screenit.service.impl.IBrievenGeneratorHelper;

import com.aspose.words.Document;

public interface BaseBriefService
{
	BriefDefinitie getBriefDefinitie(BriefType briefType, Date geldigOp);

	BriefDefinitie getNieuwsteBriefDefinitie(BriefType briefType);

	List<BriefDefinitie> getBriefDefinities(BvoZoekCriteria briefType, Comparator<BriefType> comparator);

	void saveBriefDefinitie(BriefDefinitie definitie, File uploadFile, String contentType, String filename, UnaryOperator<String> getString) throws IOException;

	BezwaarBrief maakBezwaarBrief(BezwaarMoment bezwaar, BriefType type, Date date);

	AlgemeneBrief maakAlgemeneBrief(Client client, BriefType type);

	ProjectBrief maakProjectBrief(ProjectClient pClient, ProjectBriefActie actie);

	<B extends ClientBrief<?, A, ?>, A extends Afmelding<?, ?, B>> B maakBvoBrief(A afmelding, BriefType type, Date creatieMoment, boolean vervangendeProjectBrief);

	<B extends ClientBrief<?, A, ?>, A extends Afmelding<?, ?, B>> B maakBvoBrief(A afmelding, BriefType type, Date creatieMoment);

	<B extends ClientBrief<SR, ?, ?>, SR extends ScreeningRonde<?, B, ?, ?>> B maakBvoBrief(SR ronde, BriefType type);

	<B extends ClientBrief<SR, ?, ?>, SR extends ScreeningRonde<?, B, ?, ?>> B maakBvoBrief(SR ronde, BriefType type, Date creatieMoment);

	<B extends ClientBrief<SR, ?, ?>, SR extends ScreeningRonde<?, B, ?, ?>> B maakBvoBrief(SR ronde, BriefType type, boolean gegenereerd);

	<B extends ClientBrief<SR, ?, ?>, SR extends ScreeningRonde<?, B, ?, ?>> B maakBvoBrief(SR ronde, BriefType type, Date creatieMoment, boolean gegenereerd,
		boolean vervangendeProjectBrief);

	CervixRegioBrief maakRegioBrief(ScreeningOrganisatie so, BriefType type, Date date, CervixHuisarts arts);

	<B extends ClientBrief<?, ?, ?>> void checkVoorDubbeleBrieven(BriefType type, Client client, Class<B> briefClass);

	boolean clientHeeftOngegenereerdeBriefVanType(BriefType type, Client client, Class<? extends ClientBrief<?, ?, ?>> briefClass);

	void completePdf(MergedBrieven<?> mergedBrieven);

	<B extends Brief, MB extends MergedBrieven<?>> void createOrAddMergedBrieven(List<? extends B> items, IBrievenGeneratorHelper<B, MB> briefGenerator) throws Exception;

	<B extends Brief> File maakPdfVanBrief(B brief) throws Exception;

	<B extends Brief> File maakPdfVanBrief(B brief, BaseDocumentCreator documentCreator) throws Exception;

	<B extends Brief> File maakPdfVanBrief(B brief, BaseDocumentCreator documentCreator, Consumer<MailMergeContext> mergeContextConsumer) throws Exception;

	<B extends Brief> File maakPdfVanBrief(B brief, Consumer<MailMergeContext> context) throws Exception;

	File genereerPdf(Document document, String fileNaam, boolean autoShowPrintdialog) throws FileNotFoundException, Exception;

	<B extends Brief> void setBriefGegenereerd(B brief);

	<B extends Brief> List<B> getNietGegenereerdeBrievenVanBriefTypes(List<B> brieven, List<BriefType> brieftypes);

	void setNietGegenereerdeBrievenOpTegenhouden(ScreeningRonde<?, ?, ?, ?> screeningRonde, Collection<BriefType> brieftypes);

	boolean briefTypeWachtOpKlaarzettenInDezeRonde(ClientBrief<?, ?, ?> brief);

	boolean briefTypeWachtOpKlaarzettenInDezeRonde(ScreeningRonde<?, ?, ?, ?> ronde, Collection<BriefType> brieftypes);

	boolean briefTypeAlVerstuurdInDezeRonde(ScreeningRonde<?, ?, ?, ?> ronde, Collection<BriefType> brieftypes);

	List<ClientBrief<?, ?, ?>> getClientBrieven(Client client);

	void briefTegenhouden(ClientBrief brief, Account account);

	void briefNietMeerTegenhouden(ClientBrief brief, Account account);

	boolean briefTypeGemaaktInDezeRonde(ScreeningRonde<?, ?, ?, ?> ronde, Collection<BriefType> briefTypes);
}
