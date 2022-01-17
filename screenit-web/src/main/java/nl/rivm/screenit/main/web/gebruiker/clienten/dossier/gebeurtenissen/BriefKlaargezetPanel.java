package nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.io.FileInputStream;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import javax.annotation.CheckForNull;

import nl.rivm.screenit.main.model.ScreeningRondeGebeurtenis;
import nl.rivm.screenit.main.model.TypeGebeurtenis;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.BriefTypeLabel;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.gebruiker.algemeen.documenttemplatetesten.PdfViewerPanel;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Brief;
import nl.rivm.screenit.model.ClientBrief;
import nl.rivm.screenit.model.MergedBrieven;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.cervix.CervixBrief;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaBrief;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.service.AsposeService;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.BriefHerdrukkenService;
import nl.rivm.screenit.service.FileService;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.util.BriefUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;
import org.wicketstuff.shiro.ShiroConstraint;

import com.aspose.words.Document;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = { Recht.GEBRUIKER_CLIENT_SR_BRIEVEN_OPNIEUW_KLAARZETTEN,
		Recht.GEBRUIKER_CLIENT_SR_BRIEVEN_TEGENHOUDEN, Recht.GEBRUIKER_CLIENT_SCREENINGSRONDE },
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
public class BriefKlaargezetPanel extends AbstractGebeurtenisDetailPanel
{

	private static final Logger LOG = LoggerFactory.getLogger(BriefKlaargezetPanel.class);

	@SpringBean
	private BriefHerdrukkenService briefHerdrukkenService;

	@SpringBean
	private BaseBriefService baseBriefService;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private LogService logService;

	@SpringBean
	private AsposeService asposeService;

	@SpringBean
	private FileService fileService;

	private BootstrapDialog pdfDialog = new BootstrapDialog("pdfDialog");

	private WebMarkupContainer tegenhoudenContainer;

	private final List<BriefType> tegenhoudenNietMogelijkBriefTypes = Collections.singletonList(BriefType.CERVIX_ZAS_UITNODIGING);

	public BriefKlaargezetPanel(String id, IModel<ScreeningRondeGebeurtenis> model)
	{
		super(id, model);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		Date datum = geefDatum();
		maakBriefInzienContent(datum);
		maakBriefOpnieuwAanmakenContent(datum);
		tegenhoudenContainer = maakBriefTegenhoudenContent();
		add(tegenhoudenContainer);
	}

	private Date geefDatum()
	{
		ClientBrief<?, ?, ?> brief = getModelObject().getBrief();
		MergedBrieven<?> mergedBrieven = brief.getMergedBrieven();
		if (brief.isGegenereerd() && mergedBrieven != null)
		{
			if (mergedBrieven.getPrintDatum() != null)
			{
				return mergedBrieven.getPrintDatum();
			}
			else
			{
				return mergedBrieven.getCreatieDatum();
			}
		}
		else
		{
			return brief.getCreatieDatum();
		}
	}

	private void maakBriefInzienContent(Date datum)
	{
		add(pdfDialog);

		WebMarkupContainer inzienContainer = new WebMarkupContainer("inzienContainer");
		inzienContainer.setOutputMarkupId(true);

		WebMarkupContainer inzienGroep = new WebMarkupContainer("inzienGroep");

		final ClientBrief brief = getModelObject().getBrief();
		Label inzienMsg;
		if (brief.getBriefDefinitie() != null)
		{
			inzienMsg = new Label("inzienMelding");
			inzienMsg.setVisible(false);

			final File file = fileService.load(brief.getBriefDefinitie().getDocument());
			inzienGroep.add(new IndicatingAjaxLink<Void>("inzien")
			{
				@Override
				public void onClick(AjaxRequestTarget ajaxRequestTarget)
				{
					final Document document = genereerAsposeDocument(file);
					if (document != null)
					{
						showPdf(ajaxRequestTarget, document);
					}
				}
			});
		}
		else
		{
			inzienMsg = new Label("inzienMelding", "Gebruikte template niet beschikbaar, want deze brief is " +
				(brief.getTemplateNaam() == null ? "nog niet verzonden" : "verzonden voordat ScreenIT de templates van de verzonden brieven ging bewaren, of nog niet verzonden"));
			inzienGroep.setVisible(false);
		}
		WebMarkupContainer inzienMeldingGroup = new WebMarkupContainer("inzienMeldingGroup");
		inzienMeldingGroup.setOutputMarkupId(true);
		inzienMeldingGroup.add(inzienMsg);
		inzienMeldingGroup.setVisible(inzienMsg.isVisible());
		inzienContainer.add(inzienMeldingGroup);

		voegBriefTypeOfNaamBriefToe(inzienGroep, brief);

		inzienGroep.add(DateLabel.forDatePattern("brief.creatieDatum", Model.of(datum), "dd-MM-yyyy"));
		inzienGroep.setOutputMarkupId(true);

		inzienContainer.add(inzienGroep);
		add(inzienContainer);
	}

	private void voegBriefTypeOfNaamBriefToe(WebMarkupContainer inzienGroep, Brief brief)
	{
		Label templateNaam = new Label("brief.templateNaam");
		templateNaam.setVisible(brief.getBriefType() == null);
		inzienGroep.add(templateNaam);

		BriefTypeLabel briefTypeLabel = new BriefTypeLabel("brief.briefType");
		briefTypeLabel.setVisible(brief.getBriefType() != null);
		inzienGroep.add(briefTypeLabel);
	}

	private boolean showPdf(AjaxRequestTarget target, Document document)
	{
		try
		{

			target.appendJavaScript("$('.modal.fade.in').not('#sessieVerlopenDialog').addClass('previousDialog').modal('hide');");

			pdfDialog.openWith(target, new PdfViewerPanel(IDialog.CONTENT_ID, baseBriefService.genereerPdf(document, "brieftemplate_inzien", false)));
		}
		catch (Exception e)
		{
			LOG.error(e.getMessage());
			return true;
		}
		return false;
	}

	@CheckForNull
	private Document genereerAsposeDocument(File file)
	{
		try
		{
			FileInputStream stream = new FileInputStream(file);
			return new Document(stream);
		}
		catch (Exception e)
		{
			LOG.error(e.getMessage());
			return null;
		}
	}

	private void maakBriefOpnieuwAanmakenContent(Date datum)
	{
		WebMarkupContainer opnieuwContainer = new WebMarkupContainer("opnieuwContainer");
		opnieuwContainer.setOutputMarkupId(true);
		ClientBrief<?, ?, ?> brief = getModelObject().getBrief();
		opnieuwContainer.setVisible(ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CLIENT_SR_BRIEVEN_OPNIEUW_KLAARZETTEN, Actie.AANPASSEN)
			&& !tegenhoudenNietMogelijkBriefTypes.contains(brief.getBriefType()));

		WebMarkupContainer opnieuwMogelijkContainer = new WebMarkupContainer("opnieuwMogelijk");

		voegBriefTypeOfNaamBriefToe(opnieuwMogelijkContainer, brief);

		boolean magOpnieuwAanvragen = brief.isGegenereerd() && magHerdrukken(brief) && brief.getHerdruk() == null;

		WebMarkupContainer nietOpnieuw = new WebMarkupContainer("nietOpnieuw");
		nietOpnieuw.setOutputMarkupId(true);
		String melding = getString("message.nietmogelijkbriefopnieuw");
		if (brief.getHerdruk() != null)
		{
			melding = getString("message.briefisaleenherdruk");
		}
		nietOpnieuw.add(new Label("tekstNietOpnieuw", Model.of(melding)));
		nietOpnieuw.setVisible(!magOpnieuwAanvragen);

		opnieuwMogelijkContainer.add(DateLabel.forDatePattern("brief.creatieDatum", Model.of(datum), "dd-MM-yyyy"));
		opnieuwMogelijkContainer.setOutputMarkupId(true);
		opnieuwMogelijkContainer.add(new IndicatingAjaxLink<Void>("aanmaken")
		{

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				briefHerdrukkenService.opnieuwAanmaken(BriefKlaargezetPanel.this.getModelObject().getBrief(), ScreenitSession.get().getLoggedInAccount());
				info(getString("info.briefaangemaakt"));
				opnieuwMogelijkContainer.setVisible(false);
				nietOpnieuw.setVisible(true);
				target.add(opnieuwContainer);
			}
		});

		opnieuwMogelijkContainer.setVisible(magOpnieuwAanvragen);

		opnieuwContainer.add(opnieuwMogelijkContainer);

		opnieuwContainer.add(nietOpnieuw);
		add(opnieuwContainer);
	}

	private boolean magHerdrukken(ClientBrief<?, ?, ?> brief)
	{
		brief = (ClientBrief<?, ?, ?>) HibernateHelper.deproxy(brief);
		boolean magHerdrukken = true;
		if (brief instanceof CervixBrief)
		{
			CervixBrief cervixBrief = (CervixBrief) brief;
			CervixScreeningRonde screeningRonde = cervixBrief.getScreeningRonde();
			if (screeningRonde.getStatus() == ScreeningRondeStatus.AFGEROND && cervixBrief.getUitnodiging() != null)
			{
				magHerdrukken = false;
			}
			if (magHerdrukken)
			{
				magHerdrukken = !baseBriefService.briefTypeWachtOpKlaarzettenInDezeRonde(cervixBrief);
			}
		}
		else if (brief instanceof MammaBrief)
		{
			MammaBrief mammaBrief = (MammaBrief) brief;
			MammaScreeningRonde screeningRonde = mammaBrief.getScreeningRonde();
			if (mammaBrief.getUitnodiging() != null && screeningRonde.getStatus() == ScreeningRondeStatus.AFGEROND)
			{
				magHerdrukken = false;
			}
			if (magHerdrukken)
			{
				magHerdrukken = !baseBriefService.briefTypeWachtOpKlaarzettenInDezeRonde(mammaBrief);
			}
		}

		if (magHerdrukken && brief.getHerdruk() != null)
		{
			magHerdrukken = false;
		}

		return magHerdrukken;
	}

	private WebMarkupContainer maakBriefTegenhoudenContent()
	{
		WebMarkupContainer tegenhoudenContainer = new WebMarkupContainer("tegenhoudenContainer");
		tegenhoudenContainer.setVisible(ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CLIENT_SR_BRIEVEN_TEGENHOUDEN, Actie.AANPASSEN));

		WebMarkupContainer mogelijk = new WebMarkupContainer("mogelijk");
		Brief brief = getModelObject().getBrief();

		voegBriefTypeOfNaamBriefToe(mogelijk, brief);

		mogelijk.add(DateLabel.forDatePattern("brief.creatieDatum", Model.of(brief.getCreatieDatum()), "dd-MM-yyyy"));
		boolean tegenhoudenNietMogelijkBevatBriefType = tegenhoudenNietMogelijkBriefTypes.contains(brief.getBriefType());
		mogelijk.add(new Label("nietMeer", Model.of("niet meer"))
			.setVisible(brief.isGegenereerd() || tegenhoudenNietMogelijkBevatBriefType));
		IndicatingAjaxLink<Void> tegenhoudenLink = new IndicatingAjaxLink<>("tegenhouden")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				ClientBrief<?, ?, ?> brief = BriefKlaargezetPanel.this.getModelObject().getBrief();
				brief.setTegenhouden(true);
				hibernateService.saveOrUpdate(brief);
				logService.logGebeurtenis(LogGebeurtenis.BRIEF_TEGENHOUDEN, ScreenitSession.get().getLoggedInAccount(), brief.getClient(),
					BriefUtil.getBriefTypeNaam(brief) + ", wordt tegengehouden.", brief.getBriefType().getOnderzoeken());
				info(getString("info.brieftegenhouden"));
				verversTegenhouden(target);
			}
		};
		tegenhoudenLink.setVisible(!brief.isTegenhouden() && !tegenhoudenNietMogelijkBevatBriefType);
		mogelijk.add(tegenhoudenLink);
		mogelijk.add(new IndicatingAjaxLink<Void>("activeren")
		{

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				ScreeningRondeGebeurtenis screeningRondeGebeurtenis = BriefKlaargezetPanel.this.getModelObject();
				screeningRondeGebeurtenis.setGebeurtenis(TypeGebeurtenis.BRIEF_TEGENHOUDEN);
				ClientBrief<?, ?, ?> brief = screeningRondeGebeurtenis.getBrief();
				brief.setTegenhouden(false);
				hibernateService.saveOrUpdate(brief);
				logService.logGebeurtenis(LogGebeurtenis.BRIEF_DOORVOEREN, ScreenitSession.get().getLoggedInAccount(), brief.getClient(),
					BriefUtil.getBriefTypeNaam(brief) + ", was tegengehouden en wordt nu doorgevoerd.", brief.getBriefType().getOnderzoeken());
				info(getString("info.briefactiveren"));
				verversTegenhouden(target);
			}
		}.setVisible(brief.isTegenhouden()));
		mogelijk.setVisible(!brief.isGegenereerd() && !tegenhoudenNietMogelijkBevatBriefType);
		tegenhoudenContainer.add(mogelijk);

		WebMarkupContainer nietmogelijk = new WebMarkupContainer("nietMogelijk");
		nietmogelijk.add(new Label("tekst", Model.of(getString("message.nietmogelijkbrieftegenhouden"))));

		nietmogelijk.setVisible(brief.isGegenereerd() || tegenhoudenNietMogelijkBevatBriefType);
		tegenhoudenContainer.add(nietmogelijk);
		tegenhoudenContainer.setOutputMarkupId(true);
		return tegenhoudenContainer;
	}

	private void verversTegenhouden(AjaxRequestTarget target)
	{
		WebMarkupContainer nieuw = maakBriefTegenhoudenContent();
		tegenhoudenContainer.replaceWith(nieuw);
		tegenhoudenContainer = nieuw;
		target.add(tegenhoudenContainer);
	}
}
