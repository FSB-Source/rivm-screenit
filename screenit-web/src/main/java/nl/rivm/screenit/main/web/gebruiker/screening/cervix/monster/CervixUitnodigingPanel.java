package nl.rivm.screenit.main.web.gebruiker.screening.cervix.monster;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import nl.rivm.screenit.main.service.cervix.CervixBarcodeAfdrukService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.gebruiker.algemeen.documenttemplatetesten.PdfViewer;
import nl.rivm.screenit.main.web.gebruiker.screening.cervix.monster.popup.CervixMonsterBezwaarDialog;
import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.cervix.enums.CervixMonsterType;
import nl.rivm.screenit.model.cervix.enums.CervixNietAnalyseerbaarReden;
import nl.rivm.screenit.model.cervix.enums.signaleringen.CervixMonsterSignalering;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BezwaarType;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.service.BezwaarService;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.cervix.CervixVervolgService;
import nl.rivm.screenit.service.cervix.enums.CervixVervolgTekst;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.NaamUtil;
import nl.rivm.screenit.util.cervix.CervixMonsterUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.wicket.AttributeModifier;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormSubmitBehavior;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.AbstractChoice;
import org.apache.wicket.markup.html.form.CheckBoxMultipleChoice;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.RadioChoice;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.Model;
import org.apache.wicket.request.cycle.RequestCycle;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.wicketstuff.datetime.PatternDateConverter;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;

public abstract class CervixUitnodigingPanel<M extends CervixMonster> extends GenericPanel<M>
{
	private static final Logger LOG = LoggerFactory.getLogger(CervixUitnodigingPanel.class);

	@SpringBean
	private CervixVervolgService vervolgService;

	@SpringBean
	private CervixBarcodeAfdrukService barcodeAfdrukService;

	@SpringBean
	private LogService logService;

	@SpringBean
	private BezwaarService bezwaarService;

	@SpringBean
	private HibernateService hibernateService;

	protected RadioChoice<CervixNietAnalyseerbaarReden> nietAnalyseerbaarReden;

	protected CheckBoxMultipleChoice<CervixMonsterSignalering> monsterSignaleringen;

	protected TextField<String> overigeSignalering;

	private EnumLabel<CervixVervolgTekst> vervolgTekstField;

	private WebMarkupContainer barcode;

	protected WebMarkupContainer monsterSignaleringenContainer;

	private Label reedsIngeboektLabel;

	protected BootstrapDialog dialog;

	private List<CervixMonsterSignalering> oudeSignaleringen = new ArrayList<>();

	public CervixUitnodigingPanel(String id, M monster)
	{
		super(id, ModelUtil.cModel(monster));
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		CervixUitnodiging uitnodiging = getModelObject().getUitnodiging();
		Client client = uitnodiging.getScreeningRonde().getDossier().getClient();
		BMHKLaboratorium laboratorium = (BMHKLaboratorium) HibernateHelper.deproxy(ScreenitSession.get().getInstelling());

		logService.logGebeurtenis(LogGebeurtenis.CERVIX_UITNODIGING_INGEZIEN, ScreenitSession.get().getLoggedInAccount(), client, getString("titel") + " - " + getStatus(),
			Bevolkingsonderzoek.CERVIX);

		ScreenitForm<M> form = new ScreenitForm<>("form");
		form.add(new Label("monsterId"));
		form.add(new EnumLabel<CervixMonsterType>("uitnodiging.monsterType"));

		nietAnalyseerbaarReden = new RadioChoice<>("nietAnalyseerbaarReden", CervixNietAnalyseerbaarReden.getMogelijkeRedenen(uitnodiging.getMonsterType()),
			new EnumChoiceRenderer<>());
		nietAnalyseerbaarReden.setPrefix("<label class=\"radio\">");
		nietAnalyseerbaarReden.setSuffix("</label>");
		nietAnalyseerbaarReden.setOutputMarkupId(true);
		nietAnalyseerbaarReden.setOutputMarkupPlaceholderTag(true);
		form.add(nietAnalyseerbaarReden);

		monsterSignaleringenContainer = new WebMarkupContainer("monsterSignaleringenContainer");
		form.add(monsterSignaleringenContainer);

		CervixMonster monster = uitnodiging.getMonster();
		boolean heeftDigitaalLabformulier = CervixMonsterUtil.isUitstrijkje(monster)
			&& CervixMonsterUtil.getUitstrijkje(monster).getLabformulier() != null
			&& CervixMonsterUtil.getUitstrijkje(monster).getLabformulier().getDigitaal();

		monsterSignaleringen = new CheckBoxMultipleChoice<>("signaleringen",
			CervixMonsterSignalering.getMogelijkeSignaleringen(uitnodiging.getMonsterType(), heeftDigitaalLabformulier),
			new EnumChoiceRenderer<CervixMonsterSignalering>()
			{
				@Override
				public Object getDisplayValue(CervixMonsterSignalering object)
				{
					return object.getBeschrijving();
				}
			});
		monsterSignaleringen.setLabelPosition(AbstractChoice.LabelPosition.WRAP_AFTER);
		monsterSignaleringen.add(AttributeModifier.append("style", "div.label.input: margin: 0px;"));
		monsterSignaleringenContainer.add(monsterSignaleringen);

		overigeSignalering = ComponentHelper.newTextField("overigeSignalering", 255, false);
		monsterSignaleringenContainer.add(overigeSignalering);

		boolean reedsIngeboekt = reedsIngeboekt();
		if (nuInboeken())
		{
			inboeken();
		}
		boolean ingeboektInAnderLaboratorium = getModelObject().getLaboratorium() != null && !laboratorium.equals(getModelObject().getLaboratorium());

		CervixVervolgTekst vervolgTekst = vervolgService.bepaalVervolg(getModelObject()).getVervolgTekst();

		WebMarkupContainer labformulierLaboratoriumContainer = new WebMarkupContainer("labformulierLaboratoriumContainer");
		form.add(labformulierLaboratoriumContainer);

		WebMarkupContainer monsterLaboratoriumContainer = new WebMarkupContainer("monsterLaboratoriumContainer");
		monsterLaboratoriumContainer.setVisible(ingeboektInAnderLaboratorium);
		form.add(monsterLaboratoriumContainer);
		monsterLaboratoriumContainer.add(new Label("laboratorium.naam"));

		DateLabel ontvangstdatum = new DateLabel("ontvangstdatum", new PatternDateConverter("dd-MM-yyyy HH:mm", true));
		ontvangstdatum.setOutputMarkupId(true);
		form.add(ontvangstdatum);

		form.add(new Label("uitnodiging.screeningRonde.dossier.client.persoon.naamVolledig", NaamUtil.titelVoorlettersTussenvoegselEnAanspreekAchternaam(client)));
		form.add(new Label("uitnodiging.screeningRonde.dossier.client.persoon.geboortedatum", DateUtil.getGeboortedatum(client)));
		form.add(new Label("uitnodiging.screeningRonde.dossier.client.persoon.bsn"));
		form.add(new Label("uitnodiging.screeningRonde.dossier.client.persoon.gbaAdres.gbaGemeente.screeningOrganisatie.naam"));

		vervolgTekstField = new EnumLabel<>("vervolgTekst", Model.of(vervolgTekst));
		vervolgTekstField.setOutputMarkupId(true);
		form.add(vervolgTekstField);

		WebMarkupContainer fieldset = new WebMarkupContainer("fieldset");
		fieldset.add(form);
		String cssClass = ontvangstMonster() && reedsIngeboekt ? "vervolgstap-reeds-ingeboekt" : vervolgTekst.getCssClass();
		fieldset.add(new AttributeAppender("class", cssClass));
		fieldset.setOutputMarkupId(true);
		add(fieldset);
		reedsIngeboektLabel = new Label("reedsIngeboekt", Model.of(getString("reeds.ingeboekt")));
		reedsIngeboektLabel.setOutputMarkupId(true);
		reedsIngeboektLabel.setVisible(ontvangstMonster() && reedsIngeboekt);
		form.add(reedsIngeboektLabel);

		addMonsterTypeSpecifics(form, labformulierLaboratoriumContainer, laboratorium, ingeboektInAnderLaboratorium);

		form.add(new AjaxFormSubmitBehavior("change")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				saveMonster(target);

				CervixVervolgTekst vervolgstap = vervolgService.bepaalVervolg(getModelObject()).getVervolgTekst();

				fieldset.add(new AttributeModifier("class", vervolgstap.getCssClass()));
				target.add(fieldset);

				reedsIngeboektLabel.setVisible(false);
				target.add(reedsIngeboektLabel);

				EnumLabel<CervixVervolgTekst> newVervolgstapField = new EnumLabel<>("vervolgTekst", Model.of(vervolgstap));
				newVervolgstapField.setOutputMarkupId(true);
				vervolgTekstField.replaceWith(newVervolgstapField);
				vervolgTekstField = newVervolgstapField;
				target.add(vervolgTekstField);

				focusMonsterId(target);

				setStatusDropdownChoices(target);

				target.add(ontvangstdatum);

				if (moetPrintenSignaleringen())
				{
					showBarcode(target);
				}
				else
				{
					hideBarcode(target);
				}
			}
		});

		form.add(new IndicatingAjaxLink<Void>("printMonsterId")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				showBarcode(target);
			}
		});

		barcode = getEmptyBarcodePanel();
		form.add(barcode);

		bezwaarDialog();
	}

	private boolean moetPrintenSignaleringen()
	{
		List<CervixMonsterSignalering> printSignaleringen = new ArrayList<>();
		printSignaleringen.add(CervixMonsterSignalering.BARCODE_POT_FORMULIER_ONTBREEKT);
		printSignaleringen.add(CervixMonsterSignalering.BARCODE_VERTICAAL_GEPLAKT);
		printSignaleringen.add(CervixMonsterSignalering.BARCODES_POT_FORMULIER_NIET_OVEREEN);
		printSignaleringen.add(CervixMonsterSignalering.BARCODE_ONTBREEKT_BSN_AANWEZIG);
		List<CervixMonsterSignalering> signaleringen = getModelObject().getSignaleringen();
		boolean moetPrinten = false;
		if (signaleringen != null)
		{
			boolean hadAlPrintDialoog = oudeSignaleringen.stream().anyMatch(printSignaleringen::contains);
			moetPrinten = !hadAlPrintDialoog && signaleringen.stream().anyMatch(printSignaleringen::contains);
			oudeSignaleringen = new ArrayList<>(signaleringen);
		}
		return moetPrinten;
	}

	private void bezwaarDialog()
	{
		dialog = new BootstrapDialog("dialog");
		add(dialog);
		Client client = getModelObject().getUitnodiging().getBrief().getClient();
		boolean geenGebruikLichaamsMateriaal = bezwaarService.checkBezwaarInLaatsteBezwaarMomentAanwezigIs(client,
			BezwaarType.GEEN_GEBRUIK_LICHAAMSMATERIAAL_WETENSCHAPPELIJK_ONDERZOEK);
		boolean geenSignaleringAdvies = bezwaarService.checkBezwaarInLaatsteBezwaarMomentAanwezigIs(client, BezwaarType.GEEN_SIGNALERING_VERWIJSADVIES);
		if (geenGebruikLichaamsMateriaal || geenSignaleringAdvies)
		{
			CervixMonsterBezwaarDialog cervixMonsterBezwaarDialog = new CervixMonsterBezwaarDialog(IDialog.CONTENT_ID, getModel(), geenGebruikLichaamsMateriaal,
				geenSignaleringAdvies)
			{
				@Override
				public void close(AjaxRequestTarget target)
				{
					dialog.close(target);
				}
			};
			final AjaxRequestTarget target = RequestCycle.get().find(AjaxRequestTarget.class).orElse(null);
			dialog.openWith(target, cervixMonsterBezwaarDialog);
		}
	}

	void showBarcode(AjaxRequestTarget target)
	{
		File barcodeFile = barcodeAfdrukService.saveBarcodeDocument(getModelObject().getUitnodiging());
		ScreenitSession.get().addTempFile(barcodeFile);

		PdfViewer newBarcode = new PdfViewer("barcode", barcodeFile);

		this.barcode.replaceWith(newBarcode);
		this.barcode = newBarcode;

		if (target != null)
		{
			target.add(this.barcode);
			focusMonsterId(target);
		}
		registreerBarcodeAfgedrukt(target);
	}

	private void hideBarcode(AjaxRequestTarget target)
	{
		EmptyPanel newBarcode = getEmptyBarcodePanel();

		this.barcode.replaceWith(newBarcode);
		this.barcode = newBarcode;

		target.add(this.barcode);
	}

	private EmptyPanel getEmptyBarcodePanel()
	{
		EmptyPanel barcode = new EmptyPanel("barcode");
		barcode.setVisible(false);
		barcode.setOutputMarkupPlaceholderTag(true);
		barcode.setOutputMarkupId(true);
		return barcode;
	}

	protected String getSignaleringen()
	{
		List<CervixMonsterSignalering> signaleringen = getModelObject().getSignaleringen();
		if (CollectionUtils.isNotEmpty(signaleringen))
		{
			return " (signaleringen: " + StringUtils.join(signaleringen.stream().map(CervixMonsterSignalering::getBeschrijving).collect(Collectors.toList()), ", ") + ")";
		}
		return "";
	}

	protected abstract String getStatus();

	protected abstract boolean reedsIngeboekt();

	protected abstract boolean nuInboeken();

	protected abstract void inboeken();

	protected abstract void addMonsterTypeSpecifics(ScreenitForm<M> form, WebMarkupContainer labformulierLaboratoriumContainer, BMHKLaboratorium laboratorium,
		boolean ingeboektInAnderLaboratorium);

	protected abstract void setStatusDropdownChoices(AjaxRequestTarget target);

	protected abstract void saveMonster(AjaxRequestTarget target);

	protected abstract void registreerBarcodeAfgedrukt(AjaxRequestTarget target);

	protected abstract boolean ontvangstMonster();

	protected abstract void focusMonsterId(AjaxRequestTarget target);
}
