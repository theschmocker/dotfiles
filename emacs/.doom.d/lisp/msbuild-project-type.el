;;; msbuild-project-type.el --- Dotnet solution project type data and helpers -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Jacob Schmocker
;;
;; Author: Jacob Schmocker
;; Maintainer: Jacob Schmocker
;; Homepage: https://github.com/theschmocker
;; Created: Aug 10, 2023
;; Version: 0.1.0
;; Package-Requires: ((emacs "24"))

;;; Commentary:
;;
;; Contains mappings between .NET solution file project type GUIDs and
;; human-readable names + helper functions.
;;
;;; Code:

(defvar msbuild-project-type-guid-label-alist
  '(("8BB2217D-0F2D-49D1-97BC-3654ED321F3B" . "ASP.NET 5")
    ("356CAE8B-CFD3-4221-B0A8-081A261C0C10" . "ASP.NET Core Empty")
    ("687AD6DE-2DF8-4B75-A007-DEF66CD68131" . "ASP.NET Core Web API")
    ("E27D8B1D-37A3-4EFC-AFAE-77744ED86BCA" . "ASP.NET Core Web App")
    ("065C0379-B32B-4E17-B529-0A722277FE2D" . "ASP.NET Core Web App (Model-View-Controller)")
    ("32F807D6-6071-4239-8605-A9B2205AAD60" . "ASP.NET Core with Angular")
    ("4C3A4DF3-0AAD-4113-8201-4EEEA5A70EED" . "ASP.NET Core with React.js")
    ("603C0E0B-DB56-11DC-BE95-000D561079B0" . "ASP.NET MVC 1")
    ("F85E285D-A4E0-4152-9332-AB1D724D3325" . "ASP.NET MVC 2")
    ("E53F8FEA-EAE0-44A6-8774-FFD645390401" . "ASP.NET MVC 3")
    ("E3E379DF-F4C6-4180-9B81-6769533ABE47" . "ASP.NET MVC 4")
    ("349C5851-65DF-11DA-9384-00065B846F21" . "ASP.NET MVC 5")
    ("30E03E5A-5F87-4398-9D0D-FEB397AFC92D" . "Azure Functions")
    ("14B7E1DC-C58C-427C-9728-EED16291B2DA" . "Azure Resource Group (Blank Template)")
    ("E2FF0EA2-4842-46E0-A434-C62C75BAEC67" . "Azure Resource Group (Web app)")
    ("BFBC8063-F137-4FC6-AEB4-F96101BA5C8A" . "Azure WebJob (.NET Framework)")
    ("C8A4CD56-20F4-440B-8375-78386A4431B9" . "Blazor Server App")
    ("FAE04EC0-301F-11D3-BF4B-00C04F79EFBC" . "C#")
    ("9A19103F-16F7-4668-BE54-9A1E7A4F7556" . "C# (.Net Core)")
    ("8BC9CEB8-8B4A-11D0-8D11-00A0C91BC942" . "C++")
    ("2EFF6E4D-FF75-4ADF-A9BE-74BEC0B0AFF8" . "Class Library")
    ("008A663C-3F22-40EF-81B0-012B6C27E2FB" . "Console App")
    ("A9ACE9BB-CECE-4E62-9AA4-C7E7C5BD2124" . "Database")
    ("4F174C21-8C12-11D0-8340-0000F80270F8" . "Database (other project types)")
    ("3EA9E505-35AC-4774-B492-AD1749C4943A" . "Deployment Cab")
    ("06A35CCD-C46D-44D5-987B-CF40FF872267" . "Deployment Merge Module")
    ("978C614F-708E-4E1A-B201-565925725DBA" . "Deployment Setup")
    ("AB322303-2255-48EF-A496-5904EB18DA55" . "Deployment Smart Device Cab")
    ("F135691A-BF7E-435D-8960-F99683D2D49C" . "Distributed System")
    ("BF6F8E12-879D-49E7-ADF0-5503146B24B8" . "Dynamics 2012 AX C# in AOT")
    ("82B43B9B-A64C-4715-B499-D71E9CA2BD60" . "Extensibility")
    ("F2A71F9B-5D33-465A-A702-920D77279786" . "F#")
    ("E6FDF86B-F3D1-11D4-8576-0002A516ECE8" . "J#")
    ("262852C6-CD72-467D-83FE-5EEB1973A190" . "JScript")
    ("20D4826A-C6FA-45DB-90F4-C717570B9F32" . "Legacy (2003) Smart Device (C#)")
    ("CB4CE8C6-1BDB-4DC7-A4D3-65A1999772F8" . "Legacy (2003) Smart Device (VB.NET)")
    ("8BB0C5E8-0616-4F60-8E55-A43933E57E9C" . "LightSwitch")
    ("DA98106F-DEFA-4A62-8804-0BD2F166A45D" . "Lightswitch")
    ("581633EB-B896-402F-8E60-36F3DA191C85" . "LightSwitch Project")
    ("B69E3092-B931-443C-ABE7-7E7b65f2A37F" . "Micro Framework")
    ("F85E285D-A4E0-4152-9332-AB1D724D3325" . "Model-View-Controller v2 (MVC 2)")
    ("E53F8FEA-EAE0-44A6-8774-FFD645390401" . "Model-View-Controller v3 (MVC 3)")
    ("E3E379DF-F4C6-4180-9B81-6769533ABE47" . "Model-View-Controller v4 (MVC 4)")
    ("349C5851-65DF-11DA-9384-00065B846F21" . "Model-View-Controller v5 (MVC 5)")
    ("EFBA0AD7-5A72-4C68-AF49-83D382785DCF" . "Mono for Android")
    ("86F6BF2A-E449-4B3E-813B-9ACC37E5545F" . "MonoDevelop Addin")
    ("6BC8ED88-2882-458C-8E55-DFD12B67127B" . "MonoTouch")
    ("F5B4F3BC-B597-4E2B-B552-EF5D8A32436F" . "MonoTouch Binding")
    ("C1CDDADD-2546-481F-9697-4EA41081F2FC" . "Office/SharePoint App")
    ("8DB26A54-E6C6-494F-9B32-ACBB256CD3A5" . "Platform Toolset v120")
    ("C2CAFE0E-DCE1-4D03-BBF6-18283CF86E48" . "Platform Toolset v141")
    ("786C830F-07A1-408B-BD7F-6EE04809D6DB" . "Portable Class Library")
    ("F5034706-568F-408A-B7B3-4D38C6DB8A32" . "PowerShell")
    ("66A26720-8FB5-11D2-AA7E-00C04F688DDE" . "Project Folders")
    ("593B0543-81F6-4436-BA1E-4747859CAAE2" . "SharePoint (C#)")
    ("EC05E597-79D4-47F3-ADA0-324C4F7C7484" . "SharePoint (VB.NET)")
    ("F8810EC1-6754-47FC-A15F-DFABD2E3FA90" . "SharePoint Workflow")
    ("A1591282-1198-4647-A2B1-27E5FF5F6F3B" . "Silverlight")
    ("4D628B5B-2FBC-4AA6-8C16-197242AEB884" . "Smart Device (C#)")
    ("68B1623D-7FB9-47D8-8664-7ECEA3297D4F" . "Smart Device (VB.NET)")
    ("2150E333-8FDC-42A3-9474-1A3956D46DE8" . "Solution Folder")
    ("159641D6-6404-4A2A-AE62-294DE0FE8301" . "SSIS")
    ("D183A3D8-5FD8-494B-B014-37F57B35E655" . "SSIS")
    ("C9674DCB-5085-4A16-B785-4C70DD1589BD" . "SSIS")
    ("F14B399A-7131-4C87-9E4B-1186C45EF12D" . "SSRS")
    ("D954291E-2A0B-460D-934E-DC6B0785DB48" . "Store App Universal")
    ("3AC096D0-A1C2-E12C-1390-A8335801FDAB" . "Test")
    ("A5A43C5B-DE2A-4C0C-9213-0A381AF9435A" . "Universal Windows Class Library (UWP)")
    ("F184B08F-C81C-45F6-A57F-5ABD9991F28F" . "VB.NET")
    ("C252FEB5-A946-4202-B1D4-9916A0590387" . "Visual Database Tools")
    ("54435603-DBB4-11D2-8724-00A0C9A8B90C" . "Visual Studio 2015 Installer Project Extension")
    ("A860303F-1F3F-4691-B57E-529FC101A107" . "Visual Studio Tools for Applications (VSTA)")
    ("BAA0C2D2-18E2-41B9-852F-F413020CAA33" . "Visual Studio Tools for Office (VSTO)")
    ("349C5851-65DF-11DA-9384-00065B846F21" . "Web Application")
    ("E24C65DC-7377-472B-9ABA-BC803B73C61A" . "Web Site")
    ("FAE04EC0-301F-11D3-BF4B-00C04F79EFBC" . "Windows (C#)")
    ("F184B08F-C81C-45F6-A57F-5ABD9991F28F" . "Windows (VB.NET)")
    ("8BC9CEB8-8B4A-11D0-8D11-00A0C91BC942" . "Windows (Visual C++)")
    ("C7167F0D-BC9F-4E6E-AFE1-012C56B48DB5" . "Windows Application Packaging Project (MSIX)")
    ("3D9AD99F-2412-4246-B90B-4EAA41C64699" . "Windows Communication Foundation (WCF)")
    ("76F1466A-8B6D-4E39-A767-685A06062A39" . "Windows Phone 8/8.1 Blank/Hub/Webview App")
    ("C089C8C0-30E0-4E22-80C0-CE093F111A43" . "Windows Phone 8/8.1 App (C#)")
    ("DB03555F-0C8B-43BE-9FF9-57896B3C5E56" . "Windows Phone 8/8.1 App (VB.NET)")
    ("60DC8134-EBA5-43B8-BCC9-BB4BC16C2548" . "Windows Presentation Foundation (WPF)")
    ("BC8A1FFA-BEE3-4634-8014-F334798102B3" . "Windows Store (Metro) Apps & Components")
    ("14822709-B5A1-4724-98CA-57A101D1B079" . "Workflow (C#)")
    ("D59BE175-2ED0-4C54-BE3D-CDAA9F3214C8" . "Workflow (VB.NET)")
    ("32F31D43-81CC-4C15-9DE6-3FC5453562B6" . "Workflow Foundation")
    ("EFBA0AD7-5A72-4C68-AF49-83D382785DCF" . "Xamarin.Android")
    ("6BC8ED88-2882-458C-8E55-DFD12B67127B" . "Xamarin.iOS")
    ("6D335F3A-9D43-41b4-9D22-F6F17C4BE596" . "XNA (Windows)")
    ("2DF5C3F4-5A5F-47A9-8E94-23B4456F55E2" . "XNA (XBox)")
    ("D399B71A-8929-442A-A9AC-8BEC78BB2433" . "XNA (Zune)"))
  "Mapping from Project Type GUID to label. Source:
https://github.com/JamesW75/visual-studio-project-type-guid")

(defun msbuild-project-type-label (guid)
  "Return the human-readable label of the project type associated with GUID.

If there's no matching label, return GUID itself."
  (assoc-default guid msbuild-project-type-guid-label-alist #'string= guid))

(provide 'msbuild-project-type)
;;; msbuild-project-type.el ends here
