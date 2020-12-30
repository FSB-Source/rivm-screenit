package nl.rivm.screenit.main.web.gebruiker.screening.cervix.kwaliteitsborging;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.io.IOException;
import java.util.Arrays;

import nl.rivm.screenit.dao.UitnodigingsDao;
import nl.rivm.screenit.dao.cervix.CervixKwaliteitsborgingDao;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.gebruiker.algemeen.documenttemplatetesten.PdfViewerPanel;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerBasePage;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerHoofdMenuItem;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.AsposeService;
import nl.rivm.screenit.service.BaseBriefService;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.wicketstuff.shiro.ShiroConstraint;

import com.aspose.words.Document;
import com.aspose.words.ImportFormatMode;

@SecurityConstraint(
	actie = Actie.INZIEN,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.GEBRUIKER_CERVIX_BARCODES_AFDRUKKEN,
	bevolkingsonderzoekScopes = Bevolkingsonderzoek.CERVIX)
public class CervixBarcodesAfdrukkenPage extends GebruikerBasePage
{

	private static final long serialVersionUID = 1L;

	private static final Logger LOG = LoggerFactory.getLogger(CervixBarcodesAfdrukkenPage.class);

	private IModel<Integer> aantal = new Model<>(1);

	private UploadDocument uploadDocument;

	private BootstrapDialog printDialog = new BootstrapDialog("printDialog");

	@SpringBean
	private AsposeService asposeService;

	@SpringBean
	private BaseBriefService briefSerivice;

	@SpringBean
	private UitnodigingsDao uitnodigingsDao;

	@SpringBean
	private CervixKwaliteitsborgingDao cervixKwaliteitsborgingDao;

	public CervixBarcodesAfdrukkenPage()
	{
		add(printDialog);
		Form form = new Form<>("form");
		add(form);
		ScreenitDropdown aantalDropdown = new ScreenitDropdown<>("aantal", aantal, Arrays.asList(1, 5, 10, 50));
		aantalDropdown.setRequired(true);
		form.add(aantalDropdown);

		form.add(new IndicatingAjaxSubmitLink("afdrukken")
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				boolean fout = false;
				Document gecombineerdDocument = null;
				int teller = 0;
				while (teller < aantal.getObject())
				{
					++teller;
					Document document = genereerBarcodeDocumenten();
					try
					{
						if (gecombineerdDocument == null)
						{
							gecombineerdDocument = document;
						}
						else
						{
							gecombineerdDocument.appendDocument(document, ImportFormatMode.KEEP_SOURCE_FORMATTING);
						}
					}
					catch (Exception e)
					{
						LOG.error(e.getMessage());
						fout = true;
					}
				}

				if (!fout)
				{
					fout = saveAndShowPdf(target, gecombineerdDocument);
				}

				if (fout)
				{
					form.error("Er is iets mis gegaan tijdens het genereren van de barcodes.");
				}
			}
		});
	}

	private boolean saveAndShowPdf(AjaxRequestTarget target, Document gecombineerdDocument)
	{
		try
		{
			printDialog.openWith(target, new PdfViewerPanel(IDialog.CONTENT_ID, briefSerivice.genereerPdf(gecombineerdDocument, "barcodes_controlemonsters", false)));
		}
		catch (Exception e)
		{
			LOG.error(e.getMessage());
			return true;
		}
		return false;
	}

	private Document genereerBarcodeDocumenten()
	{
		String uniekId = "Q" + StringUtils.leftPad(String.valueOf(cervixKwaliteitsborgingDao.getNextKwaliteitsborgingBarcode()), 8, '0');
		CervixUitstrijkje uitstrijkje = new CervixUitstrijkje();
		uitstrijkje.setMonsterId(uniekId);
		CervixUitnodiging uitnodiging = new CervixUitnodiging();
		uitnodiging.setUitnodigingsId(uitnodigingsDao.getNextUitnodigingsId());
		uitnodiging.setMonster(uitstrijkje);

		Document document = null;
		MailMergeContext context = new MailMergeContext();
		context.setCervixUitnodiging(uitnodiging);
		Client client = new Client();
		GbaPersoon persoon = new GbaPersoon();
		persoon.setBsn("Controlemonster");
		client.setPersoon(persoon);
		context.setClient(client);
		try
		{
			byte[] briefTemplateBytes = IOUtils.toByteArray(CervixBarcodesAfdrukkenPage.class.getResourceAsStream("/CervixUitnodigingsSticker.doc"));
			document = asposeService.processDocument(briefTemplateBytes, context);
		}
		catch (IOException e)
		{
			LOG.error(e.getMessage());
		}
		catch (Exception e)
		{
			LOG.error(e.getMessage());
		}
		return document;
	}

	@Override
	protected GebruikerHoofdMenuItem getActieveMenuItem()
	{
		return GebruikerHoofdMenuItem.CERVIX;
	}

}
